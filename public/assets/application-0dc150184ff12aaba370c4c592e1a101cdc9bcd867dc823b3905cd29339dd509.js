(function(undefined) {
  if (typeof(this.Opal) !== 'undefined') {
    console.warn('Opal already loaded. Loading twice can cause troubles, please fix your setup.');
    return this.Opal;
  }

  // The Opal object that is exposed globally
  var Opal = this.Opal = {};

  // All bridged classes - keep track to donate methods from Object
  var bridged_classes = Opal.bridged_classes = [];

  // TopScope is used for inheriting constants from the top scope
  var TopScope = function(){};

  // Opal just acts as the top scope
  TopScope.prototype = Opal;

  // To inherit scopes
  Opal.constructor = TopScope;

  // List top scope constants
  Opal.constants = [];

  // This is a useful reference to global object inside ruby files
  Opal.global = this;

  // Minify common function calls
  var $hasOwn = Opal.hasOwnProperty;
  var $slice  = Opal.slice = Array.prototype.slice;

  // Nil object id is always 4
  var nil_id = 4;

  // Generates even sequential numbers greater than 4
  // (nil_id) to serve as unique ids for ruby objects
  var unique_id = nil_id;

  // Return next unique id
  Opal.uid = function() {
    unique_id += 2;
    return unique_id;
  };

  // Table holds all class variables
  Opal.cvars = {};

  // Globals table
  Opal.gvars = {};

  // Exit function, this should be replaced by platform specific implementation
  // (See nodejs and phantom for examples)
  Opal.exit = function(status) { if (Opal.gvars.DEBUG) console.log('Exited with status '+status); };

  /**
    Get a constant on the given scope. Every class and module in Opal has a
    scope used to store, and inherit, constants. For example, the top level
    `Object` in ruby has a scope accessible as `Opal.Object.$$scope`.

    To get the `Array` class using this scope, you could use:

        Opal.Object.$$scope.get("Array")

    If a constant with the given name cannot be found, then a dispatch to the
    class/module's `#const_method` is called, which by default will raise an
    error.

    @param [String] name the name of the constant to lookup
    @returns [RubyObject]
  */
  Opal.get = function(name) {
    var constant = this[name];

    if (constant == null) {
      return this.base.$const_missing(name);
    }

    return constant;
  };

  /*
   * Create a new constants scope for the given class with the given
   * base. Constants are looked up through their parents, so the base
   * scope will be the outer scope of the new klass.
   */
  function create_scope(base, klass, id) {
    var const_alloc = function() {};
    var const_scope = const_alloc.prototype = new base.constructor();

    klass.$$scope       = const_scope;
    klass.$$base_module = base.base;

    const_scope.base        = klass;
    const_scope.constructor = const_alloc;
    const_scope.constants   = [];

    if (id) {
      klass.$$orig_scope = base;
      base[id] = base.constructor[id] = klass;
      base.constants.push(id);
    }
  }

  Opal.create_scope = create_scope;

  /*
   * A `class Foo; end` expression in ruby is compiled to call this runtime
   * method which either returns an existing class of the given name, or creates
   * a new class in the given `base` scope.
   *
   * If a constant with the given name exists, then we check to make sure that
   * it is a class and also that the superclasses match. If either of these
   * fail, then we raise a `TypeError`. Note, superklass may be null if one was
   * not specified in the ruby code.
   *
   * We pass a constructor to this method of the form `function ClassName() {}`
   * simply so that classes show up with nicely formatted names inside debuggers
   * in the web browser (or node/sprockets).
   *
   * The `base` is the current `self` value where the class is being created
   * from. We use this to get the scope for where the class should be created.
   * If `base` is an object (not a class/module), we simple get its class and
   * use that as the base instead.
   *
   * @param [Object] base where the class is being created
   * @param [Class] superklass superclass of the new class (may be null)
   * @param [String] id the name of the class to be created
   * @param [Function] constructor function to use as constructor
   * @return [Class] new or existing ruby class
   */
  Opal.klass = function(base, superklass, id, constructor) {
    // If base is an object, use its class
    if (!base.$$is_class) {
      base = base.$$class;
    }

    // Not specifying a superclass means we can assume it to be Object
    if (superklass === null) {
      superklass = ObjectClass;
    }

    var klass = base.$$scope[id];

    // If a constant exists in the scope, then we must use that
    if ($hasOwn.call(base.$$scope, id) && klass.$$orig_scope === base.$$scope) {
      // Make sure the existing constant is a class, or raise error
      if (!klass.$$is_class) {
        throw Opal.TypeError.$new(id + " is not a class");
      }

      // Make sure existing class has same superclass
      if (superklass !== klass.$$super && superklass !== ObjectClass) {
        throw Opal.TypeError.$new("superclass mismatch for class " + id);
      }
    }
    else if (typeof(superklass) === 'function') {
      // passed native constructor as superklass, so bridge it as ruby class
      return bridge_class(id, superklass, base);
    }
    else {
      // if class doesnt exist, create a new one with given superclass
      klass = boot_class(superklass, constructor);

      // name class using base (e.g. Foo or Foo::Baz)
      klass.$$name = id;

      // every class gets its own constant scope, inherited from current scope
      create_scope(base.$$scope, klass, id);

      // Name new class directly onto current scope (Opal.Foo.Baz = klass)
      base[id] = base.$$scope[id] = klass;

      // Copy all parent constants to child, unless parent is Object
      if (superklass !== ObjectClass && superklass !== BasicObjectClass) {
        donate_constants(superklass, klass);
      }

      // call .inherited() hook with new class on the superclass
      if (superklass.$inherited) {
        superklass.$inherited(klass);
      }
    }

    return klass;
  };

  // Create generic class with given superclass.
  function boot_class(superklass, constructor) {
    var alloc = boot_class_alloc(null, constructor, superklass)

    return boot_class_object(superklass, alloc);
  }

  // Make `boot_class` available to the JS-API
  Opal.boot = boot_class;

  /*
   * The class object itself (as in `Class.new`)
   *
   * @param [(Opal) Class] superklass Another class object (as in `Class.new`)
   * @param [constructor]  alloc      The constructor that holds the prototype
   *                                  that will be used for instances of the
   *                                  newly constructed class.
   */
  function boot_class_object(superklass, alloc) {
    var singleton_class = function() {};
    singleton_class.prototype = superklass.constructor.prototype;

    function OpalClass() {}
    OpalClass.prototype = new singleton_class();

    var klass = new OpalClass();

    setup_module_or_class_object(klass, OpalClass, superklass, alloc.prototype);

    // @property $$alloc This is the constructor of instances of the current
    //                   class. Its prototype will be used for method lookup
    klass.$$alloc = alloc;

    // @property $$proto.$$class Make available to instances a reference to the
    //                           class they belong to.
    klass.$$proto.$$class = klass;

    return klass;
  }

  /*
   * Adds common/required properties to a module or class object
   * (as in `Module.new` / `Class.new`)
   *
   * @param module      The module or class that needs to be prepared
   *
   * @param constructor The constructor of the module or class itself,
   *                    usually it's already assigned by using `new`. Some
   *                    ipothesis on why it's needed can be found below.
   *
   * @param superklass  The superclass of the class/module object, for modules
   *                    is `Module` (of `ModuleClass` in JS context)
   *
   * @param prototype   The prototype on which the class/module methods will
   *                    be stored.
   */
  function setup_module_or_class_object(module, constructor, superklass, prototype) {
    // @property $$id Each class is assigned a unique `id` that helps
    //                comparation and implementation of `#object_id`
    module.$$id = Opal.uid();

    // @property $$proto This is the prototype on which methods will be defined
    module.$$proto = prototype;

    // @property constructor keeps a ref to the constructor, but apparently the
    //                       constructor is already set on:
    //
    //                          `var module = new constructor` is called.
    //
    //                       Maybe there are some browsers not abiding (IE6?)
    module.constructor = constructor;

    // @property $$is_class Clearly mark this as a class-like
    module.$$is_class = true;

    // @property $$super the superclass, doesn't get changed by module inclusions
    module.$$super = superklass;

    // @property $$parent direct parent class or module
    //                    starts with the superclass, after module inclusion is
    //                    the last included module
    module.$$parent = superklass;

    // @property $$methods keeps track of methods defined on the class
    //                     but seems to be used just by `define_basic_object_method`
    //                     and for donating (Ruby) Object methods to bridged classes
    //                     TODO: check if it can be removed
    module.$$methods = [];

    // @property $$inc included modules
    module.$$inc = [];
  }

  /**
    Define new module (or return existing module). The given `base` is basically
    the current `self` value the `module` statement was defined in. If this is
    a ruby module or class, then it is used, otherwise if the base is a ruby
    object then that objects real ruby class is used (e.g. if the base is the
    main object, then the top level `Object` class is used as the base).

    If a module of the given name is already defined in the base, then that
    instance is just returned.

    If there is a class of the given name in the base, then an error is
    generated instead (cannot have a class and module of same name in same base).

    Otherwise, a new module is created in the base with the given name, and that
    new instance is returned back (to be referenced at runtime).

    @param [RubyModule or Class] base class or module this definition is inside
    @param [String] id the name of the new (or existing) module
    @returns [RubyModule]
  */
  Opal.module = function(base, id) {
    var module;

    if (!base.$$is_class) {
      base = base.$$class;
    }

    if ($hasOwn.call(base.$$scope, id)) {
      module = base.$$scope[id];

      if (!module.$$is_mod && module !== ObjectClass) {
        throw Opal.TypeError.$new(id + " is not a module");
      }
    }
    else {
      module = boot_module_object();
      module.$$name = id;

      create_scope(base.$$scope, module, id);

      // Name new module directly onto current scope (Opal.Foo.Baz = module)
      base[id] = base.$$scope[id] = module;
    }

    return module;
  };

  /*
   * Internal function to create a new module instance. This simply sets up
   * the prototype hierarchy and method tables.
   */
  function boot_module_object() {
    var mtor = function() {};
    mtor.prototype = ModuleClass.constructor.prototype;

    function module_constructor() {}
    module_constructor.prototype = new mtor();

    var module = new module_constructor();
    var module_prototype = {};

    setup_module_or_class_object(module, module_constructor, ModuleClass, module_prototype);

    module.$$is_mod = true;
    module.$$dep    = [];

    return module;
  }

  /**
    Return the singleton class for the passed object.

    If the given object alredy has a singleton class, then it will be stored on
    the object as the `$$meta` property. If this exists, then it is simply
    returned back.

    Otherwise, a new singleton object for the class or object is created, set on
    the object at `$$meta` for future use, and then returned.

    @param [RubyObject] object the ruby object
    @returns [RubyClass] the singleton class for object
  */
  Opal.get_singleton_class = function(object) {
    if (object.$$meta) {
      return object.$$meta;
    }

    if (object.$$is_class) {
      return build_class_singleton_class(object);
    }

    return build_object_singleton_class(object);
  };

  /**
    Build the singleton class for an existing class.

    NOTE: Actually in MRI a class' singleton class inherits from its
    superclass' singleton class which in turn inherits from Class.

    @param [RubyClass] klass
    @returns [RubyClass]
   */
  function build_class_singleton_class(klass) {
    var meta = new Opal.Class.$$alloc;

    meta.$$class = Opal.Class;
    meta.$$proto = klass.constructor.prototype;

    meta.$$is_singleton = true;
    meta.$$inc          = [];
    meta.$$methods      = [];
    meta.$$scope        = klass.$$scope;

    return klass.$$meta = meta;
  }

  /**
    Build the singleton class for a Ruby (non class) Object.

    @param [RubyObject] object
    @returns [RubyClass]
   */
  function build_object_singleton_class(object) {
    var orig_class = object.$$class,
        class_id   = "#<Class:#<" + orig_class.$$name + ":" + orig_class.$$id + ">>";

    var Singleton = function () {};
    var meta = Opal.boot(orig_class, Singleton);
    meta.$$name   = class_id;

    meta.$$proto  = object;
    meta.$$class  = orig_class.$$class;
    meta.$$scope  = orig_class.$$scope;
    meta.$$parent = orig_class;
    return object.$$meta = meta;
  }

  /**
    The actual inclusion of a module into a class.

    ## Class `$$parent` and `iclass`

    To handle `super` calls, every class has a `$$parent`. This parent is
    used to resolve the next class for a super call. A normal class would
    have this point to its superclass. However, if a class includes a module
    then this would need to take into account the module. The module would
    also have to then point its `$$parent` to the actual superclass. We
    cannot modify modules like this, because it might be included in more
    then one class. To fix this, we actually insert an `iclass` as the class'
    `$$parent` which can then point to the superclass. The `iclass` acts as
    a proxy to the actual module, so the `super` chain can then search it for
    the required method.

    @param [RubyModule] module the module to include
    @param [RubyClass] klass the target class to include module into
    @returns [null]
  */
  Opal.append_features = function(module, klass) {
    var included = klass.$$inc;

    // check if this module is already included in the klass
    for (var j = 0, jj = included.length; j < jj; j++) {
      if (included[j] === module) {
        return;
      }
    }

    included.push(module);
    module.$$dep.push(klass);

    // iclass
    var iclass = {
      $$name:   module.$$name,
      $$proto:  module.$$proto,
      $$parent: klass.$$parent,
      $$module: module,
      $$iclass: true
    };

    klass.$$parent = iclass;

    var donator   = module.$$proto,
        prototype = klass.$$proto,
        methods   = module.$$methods;

    for (var i = 0, length = methods.length; i < length; i++) {
      var method = methods[i], current;


      if ( prototype.hasOwnProperty(method) &&
          !(current = prototype[method]).$$donated && !current.$$stub ) {
        // if the target class already has a method of the same name defined
        // and that method was NOT donated, then it must be a method defined
        // by the class so we do not want to override it
      }
      else {
        prototype[method] = donator[method];
        prototype[method].$$donated = true;
      }
    }

    if (klass.$$dep) {
      donate_methods(klass, methods.slice(), true);
    }

    donate_constants(module, klass);
  };

  // Boot a base class (makes instances).
  function boot_class_alloc(id, constructor, superklass) {
    if (superklass) {
      var ctor = function() {};
          ctor.prototype   = superklass.$$proto || superklass.prototype;

      if (id) {
        ctor.displayName = id;
      }

      constructor.prototype = new ctor();
    }

    constructor.prototype.constructor = constructor;

    return constructor;
  }

  /*
   * Builds the class object for core classes:
   * - make the class object have a singleton class
   * - make the singleton class inherit from its parent singleton class
   *
   * @param id         [String]      the name of the class
   * @param alloc      [Function]    the constructor for the core class instances
   * @param superclass [Class alloc] the constructor of the superclass
   */
  function boot_core_class_object(id, alloc, superclass) {
    var superclass_constructor = function() {};
        superclass_constructor.prototype = superclass.prototype;

    var singleton_class = function() {};
        singleton_class.prototype = new superclass_constructor();

    singleton_class.displayName = "#<Class:"+id+">";

    // the singleton_class acts as the class object constructor
    var klass = new singleton_class();

    setup_module_or_class_object(klass, singleton_class, superclass, alloc.prototype);

    klass.$$alloc = alloc;
    klass.$$name  = id;

    // Give all instances a ref to their class
    alloc.prototype.$$class = klass;

    Opal[id] = klass;
    Opal.constants.push(id);

    return klass;
  }

  /*
   * For performance, some core ruby classes are toll-free bridged to their
   * native javascript counterparts (e.g. a ruby Array is a javascript Array).
   *
   * This method is used to setup a native constructor (e.g. Array), to have
   * its prototype act like a normal ruby class. Firstly, a new ruby class is
   * created using the native constructor so that its prototype is set as the
   * target for th new class. Note: all bridged classes are set to inherit
   * from Object.
   *
   * Bridged classes are tracked in `bridged_classes` array so that methods
   * defined on Object can be "donated" to all bridged classes. This allows
   * us to fake the inheritance of a native prototype from our Object
   * prototype.
   *
   * Example:
   *
   *    bridge_class("Proc", Function);
   *
   * @param [String] name the name of the ruby class to create
   * @param [Function] constructor native javascript constructor to use
   * @param [Object] base where the bridge class is being created. If none is supplied, the top level scope (Opal) will be used
   * @return [Class] returns new ruby class
   */
  function bridge_class(name, constructor, base) {
    var klass = boot_class_object(ObjectClass, constructor);

    klass.$$name = name;

    if (base === undefined) {
      base = Opal;
    }
    else {
      base = base.$$scope;
    }

    create_scope(base, klass, name);
    bridged_classes.push(klass);

    var object_methods = BasicObjectClass.$$methods.concat(ObjectClass.$$methods);

    for (var i = 0, len = object_methods.length; i < len; i++) {
      var meth = object_methods[i];
      constructor.prototype[meth] = ObjectClass.$$proto[meth];
    }

    add_stubs_subscriber(constructor.prototype);

    return klass;
  }

  /*
   * constant assign
   */
  Opal.casgn = function(base_module, name, value) {
    var scope = base_module.$$scope;

    if (value.$$is_class && value.$$name === nil) {
      value.$$name = name;
    }

    if (value.$$is_class) {
      value.$$base_module = base_module;
    }

    scope.constants.push(name);
    return scope[name] = value;
  };

  /*
   * constant decl
   */
  Opal.cdecl = function(base_scope, name, value) {
    base_scope.constants.push(name);
    return base_scope[name] = value;
  };

  /*
   * When a source module is included into the target module, we must also copy
   * its constants to the target.
   */
  function donate_constants(source_mod, target_mod) {
    var source_constants = source_mod.$$scope.constants,
        target_scope     = target_mod.$$scope,
        target_constants = target_scope.constants;

    for (var i = 0, length = source_constants.length; i < length; i++) {
      target_constants.push(source_constants[i]);
      target_scope[source_constants[i]] = source_mod.$$scope[source_constants[i]];
    }
  };

  /*
   * Methods stubs are used to facilitate method_missing in opal. A stub is a
   * placeholder function which just calls `method_missing` on the receiver.
   * If no method with the given name is actually defined on an object, then it
   * is obvious to say that the stub will be called instead, and then in turn
   * method_missing will be called.
   *
   * When a file in ruby gets compiled to javascript, it includes a call to
   * this function which adds stubs for every method name in the compiled file.
   * It should then be safe to assume that method_missing will work for any
   * method call detected.
   *
   * Method stubs are added to the BasicObject prototype, which every other
   * ruby object inherits, so all objects should handle method missing. A stub
   * is only added if the given property name (method name) is not already
   * defined.
   *
   * Note: all ruby methods have a `$` prefix in javascript, so all stubs will
   * have this prefix as well (to make this method more performant).
   *
   *    Opal.add_stubs(["$foo", "$bar", "$baz="]);
   *
   * All stub functions will have a private `$$stub` property set to true so
   * that other internal methods can detect if a method is just a stub or not.
   * `Kernel#respond_to?` uses this property to detect a methods presence.
   *
   * @param [Array] stubs an array of method stubs to add
   */
  Opal.add_stubs = function(stubs) {
    var subscribers = Opal.stub_subscribers;
    var subscriber;

    for (var i = 0, length = stubs.length; i < length; i++) {
      var method_name = stubs[i], stub = stub_for(method_name);

      for (var j = 0; j < subscribers.length; j++) {
        subscriber = subscribers[j];
        if (!(method_name in subscriber)) {
          subscriber[method_name] = stub;
        }
      }
    }
  };

  /*
   * Add a prototype to the subscribers list, and (TODO) add previously stubbed
   * methods.
   *
   * @param [Prototype]
   */
  function add_stubs_subscriber(prototype) {
    // TODO: Add previously stubbed methods too.
    Opal.stub_subscribers.push(prototype);
  }

  /*
   * Keep a list of prototypes that want method_missing stubs to be added.
   *
   * @default [Prototype List] BasicObject.prototype
   */
  Opal.stub_subscribers = [BasicObject.prototype];

  /*
   * Add a method_missing stub function to the given prototype for the
   * given name.
   *
   * @param [Prototype] prototype the target prototype
   * @param [String] stub stub name to add (e.g. "$foo")
   */
  function add_stub_for(prototype, stub) {
    var method_missing_stub = stub_for(stub);
    prototype[stub] = method_missing_stub;
  }

  /*
   * Generate the method_missing stub for a given method name.
   *
   * @param [String] method_name The js-name of the method to stub (e.g. "$foo")
   */
  function stub_for(method_name) {
    function method_missing_stub() {
      // Copy any given block onto the method_missing dispatcher
      this.$method_missing.$$p = method_missing_stub.$$p;

      // Set block property to null ready for the next call (stop false-positives)
      method_missing_stub.$$p = null;

      // call method missing with correct args (remove '$' prefix on method name)
      return this.$method_missing.apply(this, [method_name.slice(1)].concat($slice.call(arguments)));
    }

    method_missing_stub.$$stub = true;

    return method_missing_stub;
  }

  // Expose for other parts of Opal to use
  Opal.add_stub_for = add_stub_for;

  // Arity count error dispatcher
  Opal.ac = function(actual, expected, object, meth) {
    var inspect = (object.$$is_class ? object.$$name + '.' : object.$$class.$$name + '#') + meth;
    var msg = '[' + inspect + '] wrong number of arguments(' + actual + ' for ' + expected + ')';
    throw Opal.ArgumentError.$new(msg);
  };

  // Super dispatcher
  Opal.find_super_dispatcher = function(obj, jsid, current_func, iter, defs) {
    var dispatcher;

    if (defs) {
      dispatcher = obj.$$is_class ? defs.$$super : obj.$$class.$$proto;
    }
    else {
      if (obj.$$is_class) {
        dispatcher = obj.$$super;
      }
      else {
        dispatcher = find_obj_super_dispatcher(obj, jsid, current_func);
      }
    }

    dispatcher = dispatcher['$' + jsid];
    dispatcher.$$p = iter;

    return dispatcher;
  };

  // Iter dispatcher for super in a block
  Opal.find_iter_super_dispatcher = function(obj, jsid, current_func, iter, defs) {
    if (current_func.$$def) {
      return Opal.find_super_dispatcher(obj, current_func.$$jsid, current_func, iter, defs);
    }
    else {
      return Opal.find_super_dispatcher(obj, jsid, current_func, iter, defs);
    }
  };

  function find_obj_super_dispatcher(obj, jsid, current_func) {
    var klass = obj.$$meta || obj.$$class;
    jsid = '$' + jsid;

    while (klass) {
      if (klass.$$proto[jsid] === current_func) {
        // ok
        break;
      }

      klass = klass.$$parent;
    }

    // if we arent in a class, we couldnt find current?
    if (!klass) {
      throw new Error("could not find current class for super()");
    }

    klass = klass.$$parent;

    // else, let's find the next one
    while (klass) {
      var working = klass.$$proto[jsid];

      if (working && working !== current_func) {
        // ok
        break;
      }

      klass = klass.$$parent;
    }

    return klass.$$proto;
  };

  /*
   * Used to return as an expression. Sometimes, we can't simply return from
   * a javascript function as if we were a method, as the return is used as
   * an expression, or even inside a block which must "return" to the outer
   * method. This helper simply throws an error which is then caught by the
   * method. This approach is expensive, so it is only used when absolutely
   * needed.
   */
  Opal.ret = function(val) {
    Opal.returner.$v = val;
    throw Opal.returner;
  };

  // handles yield calls for 1 yielded arg
  Opal.yield1 = function(block, arg) {
    if (typeof(block) !== "function") {
      throw Opal.LocalJumpError.$new("no block given");
    }

    if (block.length > 1 && arg.$$is_array) {
      return block.apply(null, arg);
    }
    else {
      return block(arg);
    }
  };

  // handles yield for > 1 yielded arg
  Opal.yieldX = function(block, args) {
    if (typeof(block) !== "function") {
      throw Opal.LocalJumpError.$new("no block given");
    }

    if (block.length > 1 && args.length == 1) {
      if (args[0].$$is_array) {
        return block.apply(null, args[0]);
      }
    }

    if (!args.$$is_array) {
      args = $slice.call(args);
    }

    return block.apply(null, args);
  };

  // Finds the corresponding exception match in candidates.  Each candidate can
  // be a value, or an array of values.  Returns null if not found.
  Opal.rescue = function(exception, candidates) {
    for (var i = 0; i < candidates.length; i++) {
      var candidate = candidates[i];

      if (candidate.$$is_array) {
        var result = Opal.rescue(exception, candidate);

        if (result) {
          return result;
        }
      }
      else if (candidate['$==='](exception)) {
        return candidate;
      }
    }

    return null;
  };

  Opal.is_a = function(object, klass) {
    if (object.$$meta === klass) {
      return true;
    }

    var search = object.$$class;

    while (search) {
      if (search === klass) {
        return true;
      }

      for (var i = 0, length = search.$$inc.length; i < length; i++) {
        if (search.$$inc[i] == klass) {
          return true;
        }
      }

      search = search.$$super;
    }

    return false;
  };

  // Helper to convert the given object to an array
  Opal.to_ary = function(value) {
    if (value.$$is_array) {
      return value;
    }
    else if (value.$to_ary && !value.$to_ary.$$stub) {
      return value.$to_ary();
    }

    return [value];
  };

  /**
    Used to get a list of rest keyword arguments. Method takes the given
    keyword args, i.e. the hash literal passed to the method containing all
    keyword arguemnts passed to method, as well as the used args which are
    the names of required and optional arguments defined. This method then
    just returns all key/value pairs which have not been used, in a new
    hash literal.

    @param given_args [Hash] all kwargs given to method
    @param used_args [Object<String: true>] all keys used as named kwargs
    @return [Hash]
   */
  Opal.kwrestargs = function(given_args, used_args) {
    var keys      = [],
        map       = {},
        key       = null,
        given_map = given_args.smap;

    for (key in given_map) {
      if (!used_args[key]) {
        keys.push(key);
        map[key] = given_map[key];
      }
    }

    return Opal.hash2(keys, map);
  };

  /*
   * Call a ruby method on a ruby object with some arguments:
   *
   *   var my_array = [1, 2, 3, 4]
   *   Opal.send(my_array, 'length')     # => 4
   *   Opal.send(my_array, 'reverse!')   # => [4, 3, 2, 1]
   *
   * A missing method will be forwarded to the object via
   * method_missing.
   *
   * The result of either call with be returned.
   *
   * @param [Object] recv the ruby object
   * @param [String] mid ruby method to call
   */
  Opal.send = function(recv, mid) {
    var args = $slice.call(arguments, 2),
        func = recv['$' + mid];

    if (func) {
      return func.apply(recv, args);
    }

    return recv.$method_missing.apply(recv, [mid].concat(args));
  };

  Opal.block_send = function(recv, mid, block) {
    var args = $slice.call(arguments, 3),
        func = recv['$' + mid];

    if (func) {
      func.$$p = block;
      return func.apply(recv, args);
    }

    return recv.$method_missing.apply(recv, [mid].concat(args));
  };

  /*
   * Donate methods for a class/module
   */
  function donate_methods(klass, defined, indirect) {
    var methods = klass.$$methods, included_in = klass.$$dep;

    // if (!indirect) {
      klass.$$methods = methods.concat(defined);
    // }

    if (included_in) {
      for (var i = 0, length = included_in.length; i < length; i++) {
        var includee = included_in[i];
        var dest     = includee.$$proto;

        for (var j = 0, jj = defined.length; j < jj; j++) {
          var method = defined[j];

          dest[method] = klass.$$proto[method];
          dest[method].$$donated = true;
        }

        if (includee.$$dep) {
          donate_methods(includee, defined, true);
        }
      }
    }
  };

  /**
    Define the given method on the module.

    This also handles donating methods to all classes that include this
    module. Method conflicts are also handled here, where a class might already
    have defined a method of the same name, or another included module defined
    the same method.

    @param [RubyModule] module the module method defined on
    @param [String] jsid javascript friendly method name (e.g. "$foo")
    @param [Function] body method body of actual function
  */
  function define_module_method(module, jsid, body) {
    module.$$proto[jsid] = body;
    body.$$owner = module;

    module.$$methods.push(jsid);

    if (module.$$module_function) {
      module[jsid] = body;
    }

    var included_in = module.$$dep;

    if (included_in) {
      for (var i = 0, length = included_in.length; i < length; i++) {
        var includee = included_in[i];
        var dest = includee.$$proto;
        var current = dest[jsid];


        if (dest.hasOwnProperty(jsid) && !current.$$donated && !current.$$stub) {
          // target class has already defined the same method name - do nothing
        }
        else if (dest.hasOwnProperty(jsid) && !current.$$stub) {
          // target class includes another module that has defined this method
          var klass_includees = includee.$$inc;

          for (var j = 0, jj = klass_includees.length; j < jj; j++) {
            if (klass_includees[j] === current.$$owner) {
              var current_owner_index = j;
            }
            if (klass_includees[j] === module) {
              var module_index = j;
            }
          }

          // only redefine method on class if the module was included AFTER
          // the module which defined the current method body. Also make sure
          // a module can overwrite a method it defined before
          if (current_owner_index <= module_index) {
            dest[jsid] = body;
            dest[jsid].$$donated = true;
          }
        }
        else {
          // neither a class, or module included by class, has defined method
          dest[jsid] = body;
          dest[jsid].$$donated = true;
        }

        if (includee.$$dep) {
          donate_methods(includee, [jsid], true);
        }
      }
    }
  }

  /**
    Used to define methods on an object. This is a helper method, used by the
    compiled source to define methods on special case objects when the compiler
    can not determine the destination object, or the object is a Module
    instance. This can get called by `Module#define_method` as well.

    ## Modules

    Any method defined on a module will come through this runtime helper.
    The method is added to the module body, and the owner of the method is
    set to be the module itself. This is used later when choosing which
    method should show on a class if more than 1 included modules define
    the same method. Finally, if the module is in `module_function` mode,
    then the method is also defined onto the module itself.

    ## Classes

    This helper will only be called for classes when a method is being
    defined indirectly; either through `Module#define_method`, or by a
    literal `def` method inside an `instance_eval` or `class_eval` body. In
    either case, the method is simply added to the class' prototype. A special
    exception exists for `BasicObject` and `Object`. These two classes are
    special because they are used in toll-free bridged classes. In each of
    these two cases, extra work is required to define the methods on toll-free
    bridged class' prototypes as well.

    ## Objects

    If a simple ruby object is the object, then the method is simply just
    defined on the object as a singleton method. This would be the case when
    a method is defined inside an `instance_eval` block.

    @param [RubyObject or Class] obj the actual obj to define method for
    @param [String] jsid the javascript friendly method name (e.g. '$foo')
    @param [Function] body the literal javascript function used as method
    @returns [null]
  */
  Opal.defn = function(obj, jsid, body) {
    if (obj.$$is_mod) {
      define_module_method(obj, jsid, body);
    }
    else if (obj.$$is_class) {
      obj.$$proto[jsid] = body;

      if (obj === BasicObjectClass) {
        define_basic_object_method(jsid, body);
      }
      else if (obj === ObjectClass) {
        donate_methods(obj, [jsid]);
      }
    }
    else {
      obj[jsid] = body;
    }

    return nil;
  };

  /*
   * Define a singleton method on the given object.
   */
  Opal.defs = function(obj, jsid, body) {
    if (obj.$$is_class || obj.$$is_mod) {
      obj.constructor.prototype[jsid] = body;
    }
    else {
      obj[jsid] = body;
    }
  };

  function define_basic_object_method(jsid, body) {
    BasicObjectClass.$$methods.push(jsid);
    for (var i = 0, len = bridged_classes.length; i < len; i++) {
      bridged_classes[i].$$proto[jsid] = body;
    }
  }

  /*
   * Called to remove a method.
   */
  Opal.undef = function(obj, jsid) {
    delete obj.$$proto[jsid];
  };

  Opal.hash = function() {
    if (arguments.length == 1 && arguments[0].$$class == Opal.Hash) {
      return arguments[0];
    }

    var hash = new Opal.Hash.$$alloc(),
        keys = [],
        _map = {},
        smap = {},
        key, obj, length, khash, map;

    hash.map   = _map;
    hash.smap  = smap;
    hash.keys  = keys;

    if (arguments.length == 1) {
      if (arguments[0].$$is_array) {
        var args = arguments[0];

        for (var i = 0, ii = args.length; i < ii; i++) {
          var pair = args[i];

          if (pair.length !== 2) {
            throw Opal.ArgumentError.$new("value not of length 2: " + pair.$inspect());
          }

          key = pair[0];
          obj = pair[1];

          if (key.$$is_string) {
            khash = key;
            map = smap;
          } else {
            khash = key.$hash();
            map = _map;
          }

          if (map[khash] == null) {
            keys.push(key);
          }

          map[khash] = obj;
        }
      }
      else {
        obj = arguments[0];
        for (key in obj) {
          khash = key.$hash();
          smap[khash] = obj[khash];
          keys.push(key);
        }
      }
    }
    else {
      length = arguments.length;
      if (length % 2 !== 0) {
        throw Opal.ArgumentError.$new("odd number of arguments for Hash");
      }

      for (var j = 0; j < length; j++) {
        key = arguments[j];
        obj = arguments[++j];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        if (map[khash] == null) {
          keys.push(key);
        }

        map[khash] = obj;
      }
    }

    return hash;
  };

  /*
   * hash2 is a faster creator for hashes that just use symbols and
   * strings as keys. The map and keys array can be constructed at
   * compile time, so they are just added here by the constructor
   * function
   */
  Opal.hash2 = function(keys, map) {
    var hash = new Opal.Hash.$$alloc();

    hash.keys = keys;
    hash.map  = {};
    hash.smap = map;

    return hash;
  };

  /*
   * Create a new range instance with first and last values, and whether the
   * range excludes the last value.
   */
  Opal.range = function(first, last, exc) {
    var range         = new Opal.Range.$$alloc();
        range.begin   = first;
        range.end     = last;
        range.exclude = exc;

    return range;
  };

  // Require system
  // --------------
  (function(Opal) {
    var loaded_features = ['corelib/runtime'],
        require_table   = {'corelib/runtime': true},
        modules         = {};

    var current_dir  = '.';

    function mark_as_loaded(filename) {
      if (require_table[filename]) {
        return false;
      }

      loaded_features.push(filename);
      require_table[filename] = true;

      return true;
    }

    function normalize_loadable_path(path) {
      var parts, part, new_parts = [], SEPARATOR = '/';

      if (current_dir !== '.') {
        path = current_dir.replace(/\/*$/, '/') + path;
      }

      path = path.replace(/\.(rb|opal|js)$/, '');
      parts = path.split(SEPARATOR);

      for (var i = 0, ii = parts.length; i < ii; i++) {
        part = parts[i];
        if (part == '') continue;
        (part === '..') ? new_parts.pop() : new_parts.push(part)
      }

      return new_parts.join(SEPARATOR);
    }

    function load(path) {
      mark_as_loaded(path);

      var module = modules[path];

      if (module) {
        module(Opal);
      }
      else {
        var severity = Opal.dynamic_require_severity || 'warning';
        var message  = 'cannot load such file -- ' + path;

        if (severity === "error") {
          Opal.LoadError ? Opal.LoadError.$new(message) : function(){throw message}();
        }
        else if (severity === "warning") {
          console.warn('WARNING: LoadError: ' + message);
        }
      }

      return true;
    }

    function require(path) {
      if (require_table[path]) {
        return false;
      }

      return load(path);
    }

    Opal.modules         = modules;
    Opal.loaded_features = loaded_features;

    Opal.normalize_loadable_path = normalize_loadable_path;
    Opal.mark_as_loaded          = mark_as_loaded;

    Opal.load    = load;
    Opal.require = require;
  })(Opal);

  // Initialization
  // --------------

  // The actual class for BasicObject
  var BasicObjectClass;

  // The actual Object class
  var ObjectClass;

  // The actual Module class
  var ModuleClass;

  // The actual Class class
  var ClassClass;

  // Constructor for instances of BasicObject
  function BasicObject(){}

  // Constructor for instances of Object
  function Object(){}

  // Constructor for instances of Class
  function Class(){}

  // Constructor for instances of Module
  function Module(){}

  // Constructor for instances of NilClass (nil)
  function NilClass(){}

  // Constructors for *instances* of core objects
  boot_class_alloc('BasicObject', BasicObject);
  boot_class_alloc('Object',      Object,       BasicObject);
  boot_class_alloc('Module',      Module,       Object);
  boot_class_alloc('Class',       Class,        Module);

  // Constructors for *classes* of core objects
  BasicObjectClass = boot_core_class_object('BasicObject', BasicObject, Class);
  ObjectClass      = boot_core_class_object('Object',      Object,      BasicObjectClass.constructor);
  ModuleClass      = boot_core_class_object('Module',      Module,      ObjectClass.constructor);
  ClassClass       = boot_core_class_object('Class',       Class,       ModuleClass.constructor);

  // Fix booted classes to use their metaclass
  BasicObjectClass.$$class = ClassClass;
  ObjectClass.$$class      = ClassClass;
  ModuleClass.$$class      = ClassClass;
  ClassClass.$$class       = ClassClass;

  // Fix superclasses of booted classes
  BasicObjectClass.$$super = null;
  ObjectClass.$$super      = BasicObjectClass;
  ModuleClass.$$super      = ObjectClass;
  ClassClass.$$super       = ModuleClass;

  BasicObjectClass.$$parent = null;
  ObjectClass.$$parent      = BasicObjectClass;
  ModuleClass.$$parent      = ObjectClass;
  ClassClass.$$parent       = ModuleClass;

  // Internally, Object acts like a module as it is "included" into bridged
  // classes. In other words, we donate methods from Object into our bridged
  // classes as their prototypes don't inherit from our root Object, so they
  // act like module includes.
  ObjectClass.$$dep = bridged_classes;

  Opal.base                     = ObjectClass;
  BasicObjectClass.$$scope      = ObjectClass.$$scope = Opal;
  BasicObjectClass.$$orig_scope = ObjectClass.$$orig_scope = Opal;
  Opal.Kernel                   = ObjectClass;

  ModuleClass.$$scope      = ObjectClass.$$scope;
  ModuleClass.$$orig_scope = ObjectClass.$$orig_scope;
  ClassClass.$$scope       = ObjectClass.$$scope;
  ClassClass.$$orig_scope  = ObjectClass.$$orig_scope;

  ObjectClass.$$proto.toString = function() {
    return this.$to_s();
  };

  ObjectClass.$$proto.$require = Opal.require;

  Opal.top = new ObjectClass.$$alloc();

  // Nil
  Opal.klass(ObjectClass, ObjectClass, 'NilClass', NilClass);
  var nil = Opal.nil = new NilClass();
  nil.$$id = nil_id;
  nil.call = nil.apply = function() { throw Opal.LocalJumpError.$new('no block given'); };

  Opal.breaker  = new Error('unexpected break');
  Opal.returner = new Error('unexpected return');

  bridge_class('Array',     Array);
  bridge_class('Boolean',   Boolean);
  bridge_class('Numeric',   Number);
  bridge_class('String',    String);
  bridge_class('Proc',      Function);
  bridge_class('Exception', Error);
  bridge_class('Regexp',    RegExp);
  bridge_class('Time',      Date);

  TypeError.$$super = Error;
}).call(this);

if (typeof(global) !== 'undefined') {
  global.Opal = this.Opal;
  Opal.global = global;
}
if (typeof(window) !== 'undefined') {
  window.Opal = this.Opal;
  Opal.global = window;
}
;
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/helpers"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$new', '$class', '$===', '$respond_to?', '$raise', '$type_error', '$__send__', '$coerce_to', '$nil?', '$<=>', '$inspect']);
  return (function($base) {
    var self = $module($base, 'Opal');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defs(self, '$type_error', function(object, type, method, coerced) {
      var $a, $b, self = this;

      if (method == null) {
        method = nil
      }
      if (coerced == null) {
        coerced = nil
      }
      if ((($a = (($b = method !== false && method !== nil) ? coerced : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return $scope.get('TypeError').$new("can't convert " + (object.$class()) + " into " + (type) + " (" + (object.$class()) + "#" + (method) + " gives " + (coerced.$class()))
        } else {
        return $scope.get('TypeError').$new("no implicit conversion of " + (object.$class()) + " into " + (type))
      };
    });

    Opal.defs(self, '$coerce_to', function(object, type, method) {
      var $a, self = this;

      if ((($a = type['$==='](object)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return object};
      if ((($a = object['$respond_to?'](method)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise(self.$type_error(object, type))
      };
      return object.$__send__(method);
    });

    Opal.defs(self, '$coerce_to!', function(object, type, method) {
      var $a, self = this, coerced = nil;

      coerced = self.$coerce_to(object, type, method);
      if ((($a = type['$==='](coerced)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise(self.$type_error(object, type, method, coerced))
      };
      return coerced;
    });

    Opal.defs(self, '$coerce_to?', function(object, type, method) {
      var $a, self = this, coerced = nil;

      if ((($a = object['$respond_to?'](method)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return nil
      };
      coerced = self.$coerce_to(object, type, method);
      if ((($a = coerced['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      if ((($a = type['$==='](coerced)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise(self.$type_error(object, type, method, coerced))
      };
      return coerced;
    });

    Opal.defs(self, '$try_convert', function(object, type, method) {
      var $a, self = this;

      if ((($a = type['$==='](object)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return object};
      if ((($a = object['$respond_to?'](method)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return object.$__send__(method)
        } else {
        return nil
      };
    });

    Opal.defs(self, '$compare', function(a, b) {
      var $a, self = this, compare = nil;

      compare = a['$<=>'](b);
      if ((($a = compare === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (a.$class()) + " with " + (b.$class()) + " failed")};
      return compare;
    });

    Opal.defs(self, '$destructure', function(args) {
      var self = this;

      
      if (args.length == 1) {
        return args[0];
      }
      else if (args.$$is_array) {
        return args;
      }
      else {
        return $slice.call(args);
      }
    
    });

    Opal.defs(self, '$respond_to?', function(obj, method) {
      var self = this;

      
      if (obj == null || !obj.$$class) {
        return false;
      }
    
      return obj['$respond_to?'](method);
    });

    Opal.defs(self, '$inspect', function(obj) {
      var self = this;

      
      if (obj === undefined) {
        return "undefined";
      }
      else if (obj === null) {
        return "null";
      }
      else if (!obj.$$class) {
        return obj.toString();
      }
      else {
        return obj.$inspect();
      }
    
    });
  })(self)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/module"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$attr_reader', '$attr_writer', '$coerce_to!', '$raise', '$=~', '$[]', '$!', '$==', '$inject', '$const_get', '$split', '$const_missing', '$to_str', '$===', '$to_proc', '$lambda', '$bind', '$call', '$class', '$append_features', '$included', '$name', '$new', '$to_s', '$__id__']);
  return (function($base, $super) {
    function $Module(){};
    var self = $Module = $klass($base, $super, 'Module', $Module);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_3, TMP_5, TMP_6;

    Opal.defs(self, '$new', TMP_1 = function() {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      
      function AnonModule(){}
      var klass      = Opal.boot(Opal.Module, AnonModule);
      klass.$$name   = nil;
      klass.$$class  = Opal.Module;
      klass.$$dep    = []
      klass.$$is_mod = true;
      klass.$$proto  = {};

      // inherit scope from parent
      Opal.create_scope(Opal.Module.$$scope, klass);

      if (block !== nil) {
        var block_self = block.$$s;
        block.$$s = null;
        block.call(klass);
        block.$$s = block_self;
      }

      return klass;
    
    });

    def['$==='] = function(object) {
      var $a, self = this;

      if ((($a = object == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return false};
      return Opal.is_a(object, self);
    };

    def['$<'] = function(other) {
      var self = this;

      
      var working = self;

      while (working) {
        if (working === other) {
          return true;
        }

        working = working.$$parent;
      }

      return false;
    
    };

    def.$alias_method = function(newname, oldname) {
      var self = this;

      
      var newjsid = '$' + newname,
          body    = self.$$proto['$' + oldname];

      if (self.$$is_singleton) {
        self.$$proto[newjsid] = body;
      }
      else {
        Opal.defn(self, newjsid, body);
      }

      return self;
    
      return self;
    };

    def.$alias_native = function(mid, jsid) {
      var self = this;

      if (jsid == null) {
        jsid = mid
      }
      return self.$$proto['$' + mid] = self.$$proto[jsid];
    };

    def.$ancestors = function() {
      var self = this;

      
      var parent = self,
          result = [];

      while (parent) {
        result.push(parent);
        result = result.concat(parent.$$inc);

        parent = parent.$$super;
      }

      return result;
    
    };

    def.$append_features = function(klass) {
      var self = this;

      Opal.append_features(self, klass);
      return self;
    };

    def.$attr_accessor = function(names) {
      var $a, $b, self = this;

      names = $slice.call(arguments, 0);
      ($a = self).$attr_reader.apply($a, [].concat(names));
      return ($b = self).$attr_writer.apply($b, [].concat(names));
    };

    Opal.defn(self, '$attr', def.$attr_accessor);

    def.$attr_reader = function(names) {
      var self = this;

      names = $slice.call(arguments, 0);
      
      var proto = self.$$proto;

      for (var i = names.length - 1; i >= 0; i--) {
        var name = names[i],
            id   = '$' + name;

        // the closure here is needed because name will change at the next
        // cycle, I wish we could use let.
        var body = (function(name) {
          return function() {
            return this[name];
          };
        })(name);

        // initialize the instance variable as nil
        proto[name] = nil;

        if (self.$$is_singleton) {
          proto.constructor.prototype[id] = body;
        }
        else {
          Opal.defn(self, id, body);
        }
      }
    
      return nil;
    };

    def.$attr_writer = function(names) {
      var self = this;

      names = $slice.call(arguments, 0);
      
      var proto = self.$$proto;

      for (var i = names.length - 1; i >= 0; i--) {
        var name = names[i],
            id   = '$' + name + '=';

        // the closure here is needed because name will change at the next
        // cycle, I wish we could use let.
        var body = (function(name){
          return function(value) {
            return this[name] = value;
          }
        })(name);

        // initialize the instance variable as nil
        proto[name] = nil;

        if (self.$$is_singleton) {
          proto.constructor.prototype[id] = body;
        }
        else {
          Opal.defn(self, id, body);
        }
      }
    
      return nil;
    };

    def.$autoload = function(const$, path) {
      var self = this;

      
      var autoloaders;

      if (!(autoloaders = self.$$autoload)) {
        autoloaders = self.$$autoload = {};
      }

      autoloaders[const$] = path;
      return nil;
    ;
    };

    def.$class_variable_get = function(name) {
      var $a, self = this;

      name = $scope.get('Opal')['$coerce_to!'](name, $scope.get('String'), "to_str");
      if ((($a = name.length < 3 || name.slice(0,2) !== '@@') !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('NameError'), "class vars should start with @@")};
      
      var value = Opal.cvars[name.slice(2)];
      (function() {if ((($a = value == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$raise($scope.get('NameError'), "uninitialized class variable @@a in")
        } else {
        return nil
      }; return nil; })()
      return value;
    
    };

    def.$class_variable_set = function(name, value) {
      var $a, self = this;

      name = $scope.get('Opal')['$coerce_to!'](name, $scope.get('String'), "to_str");
      if ((($a = name.length < 3 || name.slice(0,2) !== '@@') !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('NameError'))};
      
      Opal.cvars[name.slice(2)] = value;
      return value;
    
    };

    def.$constants = function() {
      var self = this;

      return self.$$scope.constants;
    };

    def['$const_defined?'] = function(name, inherit) {
      var $a, self = this;

      if (inherit == null) {
        inherit = true
      }
      if ((($a = name['$=~'](/^[A-Z]\w*$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "wrong constant name " + (name))
      };
      
      var scopes = [self.$$scope];

      if (inherit || self === Opal.Object) {
        var parent = self.$$super;

        while (parent !== Opal.BasicObject) {
          scopes.push(parent.$$scope);

          parent = parent.$$super;
        }
      }

      for (var i = 0, length = scopes.length; i < length; i++) {
        if (scopes[i].hasOwnProperty(name)) {
          return true;
        }
      }

      return false;
    
    };

    def.$const_get = function(name, inherit) {
      var $a, $b, TMP_2, self = this;

      if (inherit == null) {
        inherit = true
      }
      if ((($a = ($b = name['$[]']("::"), $b !== false && $b !== nil ?name['$==']("::")['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = ($b = name.$split("::")).$inject, $a.$$p = (TMP_2 = function(o, c){var self = TMP_2.$$s || this;
if (o == null) o = nil;if (c == null) c = nil;
        return o.$const_get(c)}, TMP_2.$$s = self, TMP_2), $a).call($b, self)};
      if ((($a = name['$=~'](/^[A-Z]\w*$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "wrong constant name " + (name))
      };
      
      var scopes = [self.$$scope];

      if (inherit || self == Opal.Object) {
        var parent = self.$$super;

        while (parent !== Opal.BasicObject) {
          scopes.push(parent.$$scope);

          parent = parent.$$super;
        }
      }

      for (var i = 0, length = scopes.length; i < length; i++) {
        if (scopes[i].hasOwnProperty(name)) {
          return scopes[i][name];
        }
      }

      return self.$const_missing(name);
    
    };

    def.$const_missing = function(name) {
      var self = this;

      
      if (self.$$autoload) {
        var file = self.$$autoload[name];

        if (file) {
          self.$require(file);

          return self.$const_get(name);
        }
      }
    
      return self.$raise($scope.get('NameError'), "uninitialized constant " + (self) + "::" + (name));
    };

    def.$const_set = function(name, value) {
      var $a, self = this;

      if ((($a = name['$=~'](/^[A-Z]\w*$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "wrong constant name " + (name))
      };
      try {
      name = name.$to_str()
      } catch ($err) {if (true) {
        self.$raise($scope.get('TypeError'), "conversion with #to_str failed")
        }else { throw $err; }
      };
      Opal.casgn(self, name, value);
      return value;
    };

    def.$define_method = TMP_3 = function(name, method) {
      var $a, $b, $c, TMP_4, self = this, $iter = TMP_3.$$p, block = $iter || nil, $case = nil;

      TMP_3.$$p = null;
      if ((($a = method === undefined && !(block !== nil)) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "tried to create a Proc object without a block")};
      ((($a = block) !== false && $a !== nil) ? $a : block = (function() {$case = method;if ($scope.get('Proc')['$===']($case)) {return method}else if ($scope.get('Method')['$===']($case)) {return method.$to_proc()}else if ($scope.get('UnboundMethod')['$===']($case)) {return ($b = ($c = self).$lambda, $b.$$p = (TMP_4 = function(args){var self = TMP_4.$$s || this, $a, bound = nil;
args = $slice.call(arguments, 0);
      bound = method.$bind(self);
        return ($a = bound).$call.apply($a, [].concat(args));}, TMP_4.$$s = self, TMP_4), $b).call($c)}else {return self.$raise($scope.get('TypeError'), "wrong argument type " + (block.$class()) + " (expected Proc/Method)")}})());
      
      var id = '$' + name;

      block.$$jsid = name;
      block.$$s    = null;
      block.$$def  = block;

      if (self.$$is_singleton) {
        self.$$proto[id] = block;
      }
      else {
        Opal.defn(self, id, block);
      }

      return name;
    
    };

    def.$remove_method = function(name) {
      var self = this;

      Opal.undef(self, '$' + name);
      return self;
    };

    def.$include = function(mods) {
      var self = this;

      mods = $slice.call(arguments, 0);
      
      for (var i = mods.length - 1; i >= 0; i--) {
        var mod = mods[i];

        if (mod === self) {
          continue;
        }

        (mod).$append_features(self);
        (mod).$included(self);
      }
    
      return self;
    };

    def['$include?'] = function(mod) {
      var self = this;

      
      for (var cls = self; cls; cls = cls.$$super) {
        for (var i = 0; i != cls.$$inc.length; i++) {
          var mod2 = cls.$$inc[i];
          if (mod === mod2) {
            return true;
          }
        }
      }
      return false;
    
    };

    def.$instance_method = function(name) {
      var self = this;

      
      var meth = self.$$proto['$' + name];

      if (!meth || meth.$$stub) {
        self.$raise($scope.get('NameError'), "undefined method `" + (name) + "' for class `" + (self.$name()) + "'");
      }

      return $scope.get('UnboundMethod').$new(self, meth, name);
    
    };

    def.$instance_methods = function(include_super) {
      var self = this;

      if (include_super == null) {
        include_super = true
      }
      
      var methods = [],
          proto   = self.$$proto;

      for (var prop in proto) {
        if (!(prop.charAt(0) === '$')) {
          continue;
        }

        if (!(typeof(proto[prop]) === "function")) {
          continue;
        }

        if (proto[prop].$$stub) {
          continue;
        }

        if (!self.$$is_mod) {
          if (self !== Opal.BasicObject && proto[prop] === Opal.BasicObject.$$proto[prop]) {
            continue;
          }

          if (!include_super && !proto.hasOwnProperty(prop)) {
            continue;
          }

          if (!include_super && proto[prop].$$donated) {
            continue;
          }
        }

        methods.push(prop.substr(1));
      }

      return methods;
    
    };

    def.$included = function(mod) {
      var self = this;

      return nil;
    };

    def.$extended = function(mod) {
      var self = this;

      return nil;
    };

    def.$module_eval = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "no block given")
      };
      
      var old = block.$$s,
          result;

      block.$$s = null;
      result = block.call(self);
      block.$$s = old;

      return result;
    
    };

    Opal.defn(self, '$class_eval', def.$module_eval);

    def.$module_exec = TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      
      if (block === nil) {
        throw new Error("no block given");
      }

      var block_self = block.$$s, result;

      block.$$s = null;
      result = block.apply(self, $slice.call(arguments));
      block.$$s = block_self;

      return result;
    
    };

    Opal.defn(self, '$class_exec', def.$module_exec);

    def['$method_defined?'] = function(method) {
      var self = this;

      
      var body = self.$$proto['$' + method];
      return (!!body) && !body.$$stub;
    
    };

    def.$module_function = function(methods) {
      var self = this;

      methods = $slice.call(arguments, 0);
      
      if (methods.length === 0) {
        self.$$module_function = true;
      }
      else {
        for (var i = 0, length = methods.length; i < length; i++) {
          var meth = methods[i], func = self.$$proto['$' + meth];

          self.constructor.prototype['$' + meth] = func;
        }
      }

      return self;
    
    };

    def.$name = function() {
      var self = this;

      
      if (self.$$full_name) {
        return self.$$full_name;
      }

      var result = [], base = self;

      while (base) {
        if (base.$$name === nil) {
          return result.length === 0 ? nil : result.join('::');
        }

        result.unshift(base.$$name);

        base = base.$$base_module;

        if (base === Opal.Object) {
          break;
        }
      }

      if (result.length === 0) {
        return nil;
      }

      return self.$$full_name = result.join('::');
    
    };

    def.$public = function(methods) {
      var self = this;

      methods = $slice.call(arguments, 0);
      
      if (methods.length === 0) {
        self.$$module_function = false;
      }

      return nil;
    
    };

    Opal.defn(self, '$private', def.$public);

    Opal.defn(self, '$protected', def.$public);

    Opal.defn(self, '$nesting', def.$public);

    def.$private_class_method = function(name) {
      var self = this;

      return self['$' + name] || nil;
    };

    Opal.defn(self, '$public_class_method', def.$private_class_method);

    def['$private_method_defined?'] = function(obj) {
      var self = this;

      return false;
    };

    def.$private_constant = function() {
      var self = this;

      return nil;
    };

    Opal.defn(self, '$protected_method_defined?', def['$private_method_defined?']);

    Opal.defn(self, '$public_instance_methods', def.$instance_methods);

    Opal.defn(self, '$public_method_defined?', def['$method_defined?']);

    def.$remove_class_variable = function() {
      var self = this;

      return nil;
    };

    def.$remove_const = function(name) {
      var self = this;

      
      var old = self.$$scope[name];
      delete self.$$scope[name];
      return old;
    
    };

    def.$to_s = function() {
      var $a, self = this;

      return ((($a = self.$$name) !== false && $a !== nil) ? $a : "#<" + (self.$$is_mod ? 'Module' : 'Class') + ":0x" + (self.$__id__().$to_s(16)) + ">");
    };

    return (def.$undef_method = function(symbol) {
      var self = this;

      Opal.add_stub_for(self.$$proto, "$" + symbol);
      return self;
    }, nil) && 'undef_method';
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/class"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$raise', '$allocate']);
  self.$require("corelib/module");
  return (function($base, $super) {
    function $Class(){};
    var self = $Class = $klass($base, $super, 'Class', $Class);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

    Opal.defs(self, '$new', TMP_1 = function(sup) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      if (sup == null) {
        sup = $scope.get('Object')
      }
      TMP_1.$$p = null;
      
      if (!sup.$$is_class || sup.$$is_mod) {
        self.$raise($scope.get('TypeError'), "superclass must be a Class");
      }

      function AnonClass(){};
      var klass      = Opal.boot(sup, AnonClass)
      klass.$$name   = nil;
      klass.$$parent = sup;

      // inherit scope from parent
      Opal.create_scope(sup.$$scope, klass);

      sup.$inherited(klass);

      if (block !== nil) {
        var block_self = block.$$s;
        block.$$s = null;
        block.call(klass);
        block.$$s = block_self;
      }

      return klass;
    ;
    });

    def.$allocate = function() {
      var self = this;

      
      var obj = new self.$$alloc;
      obj.$$id = Opal.uid();
      return obj;
    
    };

    def.$inherited = function(cls) {
      var self = this;

      return nil;
    };

    def.$new = TMP_2 = function(args) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      
      var obj = self.$allocate();

      obj.$initialize.$$p = block;
      obj.$initialize.apply(obj, args);
      return obj;
    ;
    };

    return (def.$superclass = function() {
      var self = this;

      return self.$$super || nil;
    }, nil) && 'superclass';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/basic_object"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$raise', '$inspect']);
  return (function($base, $super) {
    function $BasicObject(){};
    var self = $BasicObject = $klass($base, $super, 'BasicObject', $BasicObject);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    Opal.defn(self, '$initialize', function() {
      var self = this;

      return nil;
    });

    Opal.defn(self, '$==', function(other) {
      var self = this;

      return self === other;
    });

    Opal.defn(self, '$__id__', function() {
      var self = this;

      return self.$$id || (self.$$id = Opal.uid());
    });

    Opal.defn(self, '$__send__', TMP_1 = function(symbol, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_1.$$p = null;
      
      var func = self['$' + symbol]

      if (func) {
        if (block !== nil) {
          func.$$p = block;
        }

        return func.apply(self, args);
      }

      if (block !== nil) {
        self.$method_missing.$$p = block;
      }

      return self.$method_missing.apply(self, [symbol].concat(args));
    
    });

    Opal.defn(self, '$!', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$equal?', def['$==']);

    Opal.defn(self, '$instance_eval', TMP_2 = function() {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        $scope.get('Kernel').$raise($scope.get('ArgumentError'), "no block given")
      };
      
      var old = block.$$s,
          result;

      block.$$s = null;
      result = block.call(self, self);
      block.$$s = old;

      return result;
    
    });

    Opal.defn(self, '$instance_exec', TMP_3 = function(args) {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        $scope.get('Kernel').$raise($scope.get('ArgumentError'), "no block given")
      };
      
      var block_self = block.$$s,
          result;

      block.$$s = null;
      result = block.apply(self, args);
      block.$$s = block_self;

      return result;
    
    });

    return (Opal.defn(self, '$method_missing', TMP_4 = function(symbol, args) {
      var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_4.$$p = null;
      return $scope.get('Kernel').$raise($scope.get('NoMethodError'), (function() {if ((($a = self.$inspect && !self.$inspect.$$stub) !== nil && (!$a.$$is_boolean || $a == true))) {
        return "undefined method `" + (symbol) + "' for " + (self.$inspect()) + ":" + (self.$$class)
        } else {
        return "undefined method `" + (symbol) + "' for " + (self.$$class)
      }; return nil; })());
    }), nil) && 'method_missing';
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/kernel"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_gt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs > rhs : lhs['$>'](rhs);
  }
  function $rb_le(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs <= rhs : lhs['$<='](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $gvars = Opal.gvars, $hash2 = Opal.hash2;

  Opal.add_stubs(['$raise', '$inspect', '$==', '$object_id', '$class', '$new', '$coerce_to?', '$<<', '$allocate', '$copy_instance_variables', '$initialize_clone', '$initialize_copy', '$singleton_class', '$initialize_dup', '$for', '$to_proc', '$each', '$reverse', '$append_features', '$extended', '$length', '$respond_to?', '$[]', '$nil?', '$to_a', '$to_int', '$fetch', '$Integer', '$Float', '$to_ary', '$to_str', '$coerce_to', '$to_s', '$__id__', '$coerce_to!', '$===', '$print', '$format', '$puts', '$empty?', '$rand', '$respond_to_missing?', '$try_convert!', '$expand_path', '$join', '$start_with?']);
  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_9;

    Opal.defn(self, '$method_missing', TMP_1 = function(symbol, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_1.$$p = null;
      return self.$raise($scope.get('NoMethodError'), "undefined method `" + (symbol) + "' for " + (self.$inspect()));
    });

    Opal.defn(self, '$=~', function(obj) {
      var self = this;

      return false;
    });

    Opal.defn(self, '$===', function(other) {
      var $a, self = this;

      return ((($a = self.$object_id()['$=='](other.$object_id())) !== false && $a !== nil) ? $a : self['$=='](other));
    });

    Opal.defn(self, '$<=>', function(other) {
      var self = this;

      
      var x = self['$=='](other);

      if (x && x !== nil) {
        return 0;
      }

      return nil;
    ;
    });

    Opal.defn(self, '$method', function(name) {
      var self = this;

      
      var meth = self['$' + name];

      if (!meth || meth.$$stub) {
        self.$raise($scope.get('NameError'), "undefined method `" + (name) + "' for class `" + (self.$class()) + "'");
      }

      return $scope.get('Method').$new(self, meth, name);
    
    });

    Opal.defn(self, '$methods', function(all) {
      var self = this;

      if (all == null) {
        all = true
      }
      
      var methods = [];

      for (var key in self) {
        if (key[0] == "$" && typeof(self[key]) === "function") {
          if (all == false || all === nil) {
            if (!Opal.hasOwnProperty.call(self, key)) {
              continue;
            }
          }
          if (self[key].$$stub === undefined) {
            methods.push(key.substr(1));
          }
        }
      }

      return methods;
    
    });

    Opal.defn(self, '$Array', function(object) {
      var self = this;

      
      var coerced;

      if (object === nil) {
        return [];
      }

      if (object.$$is_array) {
        return object;
      }

      coerced = $scope.get('Opal')['$coerce_to?'](object, $scope.get('Array'), "to_ary");
      if (coerced !== nil) { return coerced; }

      coerced = $scope.get('Opal')['$coerce_to?'](object, $scope.get('Array'), "to_a");
      if (coerced !== nil) { return coerced; }

      return [object];
    
    });

    Opal.defn(self, '$at_exit', TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;
      if ($gvars.__at_exit__ == null) $gvars.__at_exit__ = nil;

      TMP_2.$$p = null;
      ((($a = $gvars.__at_exit__) !== false && $a !== nil) ? $a : $gvars.__at_exit__ = []);
      return $gvars.__at_exit__['$<<'](block);
    });

    Opal.defn(self, '$caller', function() {
      var self = this;

      return [];
    });

    Opal.defn(self, '$class', function() {
      var self = this;

      return self.$$class;
    });

    Opal.defn(self, '$copy_instance_variables', function(other) {
      var self = this;

      
      for (var name in other) {
        if (name.charAt(0) !== '$') {
          self[name] = other[name];
        }
      }
    
    });

    Opal.defn(self, '$clone', function() {
      var self = this, copy = nil;

      copy = self.$class().$allocate();
      copy.$copy_instance_variables(self);
      copy.$initialize_clone(self);
      return copy;
    });

    Opal.defn(self, '$initialize_clone', function(other) {
      var self = this;

      return self.$initialize_copy(other);
    });

    Opal.defn(self, '$define_singleton_method', TMP_3 = function(name, body) {
      var $a, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      if (body == null) {
        body = nil
      }
      TMP_3.$$p = null;
      ((($a = body) !== false && $a !== nil) ? $a : body = block);
      if (body !== false && body !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "tried to create Proc object without a block")
      };
      
      var jsid   = '$' + name;
      body.$$jsid = name;
      body.$$s    = null;
      body.$$def  = body;

      self.$singleton_class().$$proto[jsid] = body;

      return self;
    
    });

    Opal.defn(self, '$dup', function() {
      var self = this, copy = nil;

      copy = self.$class().$allocate();
      copy.$copy_instance_variables(self);
      copy.$initialize_dup(self);
      return copy;
    });

    Opal.defn(self, '$initialize_dup', function(other) {
      var self = this;

      return self.$initialize_copy(other);
    });

    Opal.defn(self, '$enum_for', TMP_4 = function(method, args) {
      var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      if (method == null) {
        method = "each"
      }
      TMP_4.$$p = null;
      return ($a = ($b = $scope.get('Enumerator')).$for, $a.$$p = block.$to_proc(), $a).apply($b, [self, method].concat(args));
    });

    Opal.defn(self, '$to_enum', def.$enum_for);

    Opal.defn(self, '$equal?', function(other) {
      var self = this;

      return self === other;
    });

    Opal.defn(self, '$exit', function(status) {
      var $a, $b, self = this;
      if ($gvars.__at_exit__ == null) $gvars.__at_exit__ = nil;

      if (status == null) {
        status = true
      }
      if ((($a = $gvars.__at_exit__) !== nil && (!$a.$$is_boolean || $a == true))) {
        ($a = ($b = $gvars.__at_exit__.$reverse()).$each, $a.$$p = "call".$to_proc(), $a).call($b)};
      if ((($a = status === true) !== nil && (!$a.$$is_boolean || $a == true))) {
        status = 0};
      Opal.exit(status);
      return nil;
    });

    Opal.defn(self, '$extend', function(mods) {
      var self = this;

      mods = $slice.call(arguments, 0);
      
      var singleton = self.$singleton_class();

      for (var i = mods.length - 1; i >= 0; i--) {
        var mod = mods[i];

        (mod).$append_features(singleton);
        (mod).$extended(self);
      }
    ;
      return self;
    });

    Opal.defn(self, '$format', function(format_string, args) {
      var $a, $b, self = this, ary = nil;
      if ($gvars.DEBUG == null) $gvars.DEBUG = nil;

      args = $slice.call(arguments, 1);
      if ((($a = (($b = args.$length()['$=='](1)) ? args['$[]'](0)['$respond_to?']("to_ary") : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        ary = $scope.get('Opal')['$coerce_to?'](args['$[]'](0), $scope.get('Array'), "to_ary");
        if ((($a = ary['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          args = ary.$to_a()
        };};
      
      var result = '',
          //used for slicing:
          begin_slice = 0,
          end_slice,
          //used for iterating over the format string:
          i,
          len = format_string.length,
          //used for processing field values:
          arg,
          str,
          //used for processing %g and %G fields:
          exponent,
          //used for keeping track of width and precision:
          width,
          precision,
          //used for holding temporary values:
          tmp_num,
          //used for processing %{} and %<> fileds:
          hash_parameter_key,
          closing_brace_char,
          //used for processing %b, %B, %o, %x, and %X fields:
          base_number,
          base_prefix,
          base_neg_zero_regex,
          base_neg_zero_digit,
          //used for processing arguments:
          next_arg,
          seq_arg_num = 1,
          pos_arg_num = 0,
          //used for keeping track of flags:
          flags,
          FNONE  = 0,
          FSHARP = 1,
          FMINUS = 2,
          FPLUS  = 4,
          FZERO  = 8,
          FSPACE = 16,
          FWIDTH = 32,
          FPREC  = 64,
          FPREC0 = 128;

      function CHECK_FOR_FLAGS() {
        if (flags&FWIDTH) { self.$raise($scope.get('ArgumentError'), "flag after width") }
        if (flags&FPREC0) { self.$raise($scope.get('ArgumentError'), "flag after precision") }
      }

      function CHECK_FOR_WIDTH() {
        if (flags&FWIDTH) { self.$raise($scope.get('ArgumentError'), "width given twice") }
        if (flags&FPREC0) { self.$raise($scope.get('ArgumentError'), "width after precision") }
      }

      function GET_NTH_ARG(num) {
        if (num >= args.length) { self.$raise($scope.get('ArgumentError'), "too few arguments") }
        return args[num];
      }

      function GET_NEXT_ARG() {
        switch (pos_arg_num) {
        case -1: self.$raise($scope.get('ArgumentError'), "unnumbered(" + (seq_arg_num) + ") mixed with numbered")
        case -2: self.$raise($scope.get('ArgumentError'), "unnumbered(" + (seq_arg_num) + ") mixed with named")
        }
        pos_arg_num = seq_arg_num++;
        return GET_NTH_ARG(pos_arg_num - 1);
      }

      function GET_POS_ARG(num) {
        if (pos_arg_num > 0) {
          self.$raise($scope.get('ArgumentError'), "numbered(" + (num) + ") after unnumbered(" + (pos_arg_num) + ")")
        }
        if (pos_arg_num === -2) {
          self.$raise($scope.get('ArgumentError'), "numbered(" + (num) + ") after named")
        }
        if (num < 1) {
          self.$raise($scope.get('ArgumentError'), "invalid index - " + (num) + "$")
        }
        pos_arg_num = -1;
        return GET_NTH_ARG(num - 1);
      }

      function GET_ARG() {
        return (next_arg === undefined ? GET_NEXT_ARG() : next_arg);
      }

      function READ_NUM(label) {
        var num, str = '';
        for (;; i++) {
          if (i === len) {
            self.$raise($scope.get('ArgumentError'), "malformed format string - %*[0-9]")
          }
          if (format_string.charCodeAt(i) < 48 || format_string.charCodeAt(i) > 57) {
            i--;
            num = parseInt(str) || 0;
            if (num > 2147483647) {
              self.$raise($scope.get('ArgumentError'), "" + (label) + " too big")
            }
            return num;
          }
          str += format_string.charAt(i);
        }
      }

      function READ_NUM_AFTER_ASTER(label) {
        var arg, num = READ_NUM(label);
        if (format_string.charAt(i + 1) === '$') {
          i++;
          arg = GET_POS_ARG(num);
        } else {
          arg = GET_NEXT_ARG();
        }
        return (arg).$to_int();
      }

      for (i = format_string.indexOf('%'); i !== -1; i = format_string.indexOf('%', i)) {
        str = undefined;

        flags = FNONE;
        width = -1;
        precision = -1;
        next_arg = undefined;

        end_slice = i;

        i++;

        switch (format_string.charAt(i)) {
        case '%':
          begin_slice = i;
        case '':
        case '\n':
        case '\0':
          i++;
          continue;
        }

        format_sequence: for (; i < len; i++) {
          switch (format_string.charAt(i)) {

          case ' ':
            CHECK_FOR_FLAGS();
            flags |= FSPACE;
            continue format_sequence;

          case '#':
            CHECK_FOR_FLAGS();
            flags |= FSHARP;
            continue format_sequence;

          case '+':
            CHECK_FOR_FLAGS();
            flags |= FPLUS;
            continue format_sequence;

          case '-':
            CHECK_FOR_FLAGS();
            flags |= FMINUS;
            continue format_sequence;

          case '0':
            CHECK_FOR_FLAGS();
            flags |= FZERO;
            continue format_sequence;

          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            tmp_num = READ_NUM('width');
            if (format_string.charAt(i + 1) === '$') {
              if (i + 2 === len) {
                str = '%';
                i++;
                break format_sequence;
              }
              if (next_arg !== undefined) {
                self.$raise($scope.get('ArgumentError'), "value given twice - %" + (tmp_num) + "$")
              }
              next_arg = GET_POS_ARG(tmp_num);
              i++;
            } else {
              CHECK_FOR_WIDTH();
              flags |= FWIDTH;
              width = tmp_num;
            }
            continue format_sequence;

          case '<':
          case '\{':
            closing_brace_char = (format_string.charAt(i) === '<' ? '>' : '\}');
            hash_parameter_key = '';

            i++;

            for (;; i++) {
              if (i === len) {
                self.$raise($scope.get('ArgumentError'), "malformed name - unmatched parenthesis")
              }
              if (format_string.charAt(i) === closing_brace_char) {

                if (pos_arg_num > 0) {
                  self.$raise($scope.get('ArgumentError'), "named " + (hash_parameter_key) + " after unnumbered(" + (pos_arg_num) + ")")
                }
                if (pos_arg_num === -1) {
                  self.$raise($scope.get('ArgumentError'), "named " + (hash_parameter_key) + " after numbered")
                }
                pos_arg_num = -2;

                if (args[0] === undefined || !args[0].$$is_hash) {
                  self.$raise($scope.get('ArgumentError'), "one hash required")
                }

                next_arg = (args[0]).$fetch(hash_parameter_key);

                if (closing_brace_char === '>') {
                  continue format_sequence;
                } else {
                  str = next_arg.toString();
                  if (precision !== -1) { str = str.slice(0, precision); }
                  if (flags&FMINUS) {
                    while (str.length < width) { str = str + ' '; }
                  } else {
                    while (str.length < width) { str = ' ' + str; }
                  }
                  break format_sequence;
                }
              }
              hash_parameter_key += format_string.charAt(i);
            }

          case '*':
            i++;
            CHECK_FOR_WIDTH();
            flags |= FWIDTH;
            width = READ_NUM_AFTER_ASTER('width');
            if (width < 0) {
              flags |= FMINUS;
              width = -width;
            }
            continue format_sequence;

          case '.':
            if (flags&FPREC0) {
              self.$raise($scope.get('ArgumentError'), "precision given twice")
            }
            flags |= FPREC|FPREC0;
            precision = 0;
            i++;
            if (format_string.charAt(i) === '*') {
              i++;
              precision = READ_NUM_AFTER_ASTER('precision');
              if (precision < 0) {
                flags &= ~FPREC;
              }
              continue format_sequence;
            }
            precision = READ_NUM('precision');
            continue format_sequence;

          case 'd':
          case 'i':
          case 'u':
            arg = self.$Integer(GET_ARG());
            if (arg >= 0) {
              str = arg.toString();
              while (str.length < precision) { str = '0' + str; }
              if (flags&FMINUS) {
                if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                while (str.length < width) { str = str + ' '; }
              } else {
                if (flags&FZERO && precision === -1) {
                  while (str.length < width - ((flags&FPLUS || flags&FSPACE) ? 1 : 0)) { str = '0' + str; }
                  if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                } else {
                  if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                  while (str.length < width) { str = ' ' + str; }
                }
              }
            } else {
              str = (-arg).toString();
              while (str.length < precision) { str = '0' + str; }
              if (flags&FMINUS) {
                str = '-' + str;
                while (str.length < width) { str = str + ' '; }
              } else {
                if (flags&FZERO && precision === -1) {
                  while (str.length < width - 1) { str = '0' + str; }
                  str = '-' + str;
                } else {
                  str = '-' + str;
                  while (str.length < width) { str = ' ' + str; }
                }
              }
            }
            break format_sequence;

          case 'b':
          case 'B':
          case 'o':
          case 'x':
          case 'X':
            switch (format_string.charAt(i)) {
            case 'b':
            case 'B':
              base_number = 2;
              base_prefix = '0b';
              base_neg_zero_regex = /^1+/;
              base_neg_zero_digit = '1';
              break;
            case 'o':
              base_number = 8;
              base_prefix = '0';
              base_neg_zero_regex = /^3?7+/;
              base_neg_zero_digit = '7';
              break;
            case 'x':
            case 'X':
              base_number = 16;
              base_prefix = '0x';
              base_neg_zero_regex = /^f+/;
              base_neg_zero_digit = 'f';
              break;
            }
            arg = self.$Integer(GET_ARG());
            if (arg >= 0) {
              str = arg.toString(base_number);
              while (str.length < precision) { str = '0' + str; }
              if (flags&FMINUS) {
                if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                if (flags&FSHARP && arg !== 0) { str = base_prefix + str; }
                while (str.length < width) { str = str + ' '; }
              } else {
                if (flags&FZERO && precision === -1) {
                  while (str.length < width - ((flags&FPLUS || flags&FSPACE) ? 1 : 0) - ((flags&FSHARP && arg !== 0) ? base_prefix.length : 0)) { str = '0' + str; }
                  if (flags&FSHARP && arg !== 0) { str = base_prefix + str; }
                  if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                } else {
                  if (flags&FSHARP && arg !== 0) { str = base_prefix + str; }
                  if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                  while (str.length < width) { str = ' ' + str; }
                }
              }
            } else {
              if (flags&FPLUS || flags&FSPACE) {
                str = (-arg).toString(base_number);
                while (str.length < precision) { str = '0' + str; }
                if (flags&FMINUS) {
                  if (flags&FSHARP) { str = base_prefix + str; }
                  str = '-' + str;
                  while (str.length < width) { str = str + ' '; }
                } else {
                  if (flags&FZERO && precision === -1) {
                    while (str.length < width - 1 - (flags&FSHARP ? 2 : 0)) { str = '0' + str; }
                    if (flags&FSHARP) { str = base_prefix + str; }
                    str = '-' + str;
                  } else {
                    if (flags&FSHARP) { str = base_prefix + str; }
                    str = '-' + str;
                    while (str.length < width) { str = ' ' + str; }
                  }
                }
              } else {
                str = (arg >>> 0).toString(base_number).replace(base_neg_zero_regex, base_neg_zero_digit);
                while (str.length < precision - 2) { str = base_neg_zero_digit + str; }
                if (flags&FMINUS) {
                  str = '..' + str;
                  if (flags&FSHARP) { str = base_prefix + str; }
                  while (str.length < width) { str = str + ' '; }
                } else {
                  if (flags&FZERO && precision === -1) {
                    while (str.length < width - 2 - (flags&FSHARP ? base_prefix.length : 0)) { str = base_neg_zero_digit + str; }
                    str = '..' + str;
                    if (flags&FSHARP) { str = base_prefix + str; }
                  } else {
                    str = '..' + str;
                    if (flags&FSHARP) { str = base_prefix + str; }
                    while (str.length < width) { str = ' ' + str; }
                  }
                }
              }
            }
            if (format_string.charAt(i) === format_string.charAt(i).toUpperCase()) {
              str = str.toUpperCase();
            }
            break format_sequence;

          case 'f':
          case 'e':
          case 'E':
          case 'g':
          case 'G':
            arg = self.$Float(GET_ARG());
            if (arg >= 0 || isNaN(arg)) {
              if (arg === Infinity) {
                str = 'Inf';
              } else {
                switch (format_string.charAt(i)) {
                case 'f':
                  str = arg.toFixed(precision === -1 ? 6 : precision);
                  break;
                case 'e':
                case 'E':
                  str = arg.toExponential(precision === -1 ? 6 : precision);
                  break;
                case 'g':
                case 'G':
                  str = arg.toExponential();
                  exponent = parseInt(str.split('e')[1]);
                  if (!(exponent < -4 || exponent >= (precision === -1 ? 6 : precision))) {
                    str = arg.toPrecision(precision === -1 ? (flags&FSHARP ? 6 : undefined) : precision);
                  }
                  break;
                }
              }
              if (flags&FMINUS) {
                if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                while (str.length < width) { str = str + ' '; }
              } else {
                if (flags&FZERO && arg !== Infinity && !isNaN(arg)) {
                  while (str.length < width - ((flags&FPLUS || flags&FSPACE) ? 1 : 0)) { str = '0' + str; }
                  if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                } else {
                  if (flags&FPLUS || flags&FSPACE) { str = (flags&FPLUS ? '+' : ' ') + str; }
                  while (str.length < width) { str = ' ' + str; }
                }
              }
            } else {
              if (arg === -Infinity) {
                str = 'Inf';
              } else {
                switch (format_string.charAt(i)) {
                case 'f':
                  str = (-arg).toFixed(precision === -1 ? 6 : precision);
                  break;
                case 'e':
                case 'E':
                  str = (-arg).toExponential(precision === -1 ? 6 : precision);
                  break;
                case 'g':
                case 'G':
                  str = (-arg).toExponential();
                  exponent = parseInt(str.split('e')[1]);
                  if (!(exponent < -4 || exponent >= (precision === -1 ? 6 : precision))) {
                    str = (-arg).toPrecision(precision === -1 ? (flags&FSHARP ? 6 : undefined) : precision);
                  }
                  break;
                }
              }
              if (flags&FMINUS) {
                str = '-' + str;
                while (str.length < width) { str = str + ' '; }
              } else {
                if (flags&FZERO && arg !== -Infinity) {
                  while (str.length < width - 1) { str = '0' + str; }
                  str = '-' + str;
                } else {
                  str = '-' + str;
                  while (str.length < width) { str = ' ' + str; }
                }
              }
            }
            if (format_string.charAt(i) === format_string.charAt(i).toUpperCase() && arg !== Infinity && arg !== -Infinity && !isNaN(arg)) {
              str = str.toUpperCase();
            }
            str = str.replace(/([eE][-+]?)([0-9])$/, '$10$2');
            break format_sequence;

          case 'a':
          case 'A':
            // Not implemented because there are no specs for this field type.
            self.$raise($scope.get('NotImplementedError'), "`A` and `a` format field types are not implemented in Opal yet")

          case 'c':
            arg = GET_ARG();
            if ((arg)['$respond_to?']("to_ary")) { arg = (arg).$to_ary()[0]; }
            if ((arg)['$respond_to?']("to_str")) {
              str = (arg).$to_str();
            } else {
              str = String.fromCharCode($scope.get('Opal').$coerce_to(arg, $scope.get('Integer'), "to_int"));
            }
            if (str.length !== 1) {
              self.$raise($scope.get('ArgumentError'), "%c requires a character")
            }
            if (flags&FMINUS) {
              while (str.length < width) { str = str + ' '; }
            } else {
              while (str.length < width) { str = ' ' + str; }
            }
            break format_sequence;

          case 'p':
            str = (GET_ARG()).$inspect();
            if (precision !== -1) { str = str.slice(0, precision); }
            if (flags&FMINUS) {
              while (str.length < width) { str = str + ' '; }
            } else {
              while (str.length < width) { str = ' ' + str; }
            }
            break format_sequence;

          case 's':
            str = (GET_ARG()).$to_s();
            if (precision !== -1) { str = str.slice(0, precision); }
            if (flags&FMINUS) {
              while (str.length < width) { str = str + ' '; }
            } else {
              while (str.length < width) { str = ' ' + str; }
            }
            break format_sequence;

          default:
            self.$raise($scope.get('ArgumentError'), "malformed format string - %" + (format_string.charAt(i)))
          }
        }

        if (str === undefined) {
          self.$raise($scope.get('ArgumentError'), "malformed format string - %")
        }

        result += format_string.slice(begin_slice, end_slice) + str;
        begin_slice = i + 1;
      }

      if ($gvars.DEBUG && pos_arg_num >= 0 && seq_arg_num < args.length) {
        self.$raise($scope.get('ArgumentError'), "too many arguments for format string")
      }

      return result + format_string.slice(begin_slice);
    ;
    });

    Opal.defn(self, '$freeze', function() {
      var self = this;

      self.___frozen___ = true;
      return self;
    });

    Opal.defn(self, '$frozen?', function() {
      var $a, self = this;
      if (self.___frozen___ == null) self.___frozen___ = nil;

      return ((($a = self.___frozen___) !== false && $a !== nil) ? $a : false);
    });

    Opal.defn(self, '$hash', function() {
      var self = this;

      return "" + (self.$class()) + ":" + (self.$class().$__id__()) + ":" + (self.$__id__());
    });

    Opal.defn(self, '$initialize_copy', function(other) {
      var self = this;

      return nil;
    });

    Opal.defn(self, '$inspect', function() {
      var self = this;

      return self.$to_s();
    });

    Opal.defn(self, '$instance_of?', function(klass) {
      var self = this;

      return self.$$class === klass;
    });

    Opal.defn(self, '$instance_variable_defined?', function(name) {
      var self = this;

      return Opal.hasOwnProperty.call(self, name.substr(1));
    });

    Opal.defn(self, '$instance_variable_get', function(name) {
      var self = this;

      
      var ivar = self[name.substr(1)];

      return ivar == null ? nil : ivar;
    
    });

    Opal.defn(self, '$instance_variable_set', function(name, value) {
      var self = this;

      return self[name.substr(1)] = value;
    });

    Opal.defn(self, '$instance_variables', function() {
      var self = this;

      
      var result = [];

      for (var name in self) {
        if (name.charAt(0) !== '$') {
          if (name !== '$$class' && name !== '$$id') {
            result.push('@' + name);
          }
        }
      }

      return result;
    
    });

    Opal.defn(self, '$Integer', function(value, base) {
      var self = this;

      
      var i, str, base_digits;

      if (!value.$$is_string) {
        if (base !== undefined) {
          self.$raise($scope.get('ArgumentError'), "base specified for non string value")
        }
        if (value === nil) {
          self.$raise($scope.get('TypeError'), "can't convert nil into Integer")
        }
        if (value.$$is_number) {
          if (value === Infinity || value === -Infinity || isNaN(value)) {
            self.$raise($scope.get('FloatDomainError'), value)
          }
          return Math.floor(value);
        }
        if (value['$respond_to?']("to_int")) {
          i = value.$to_int();
          if (i !== nil) {
            return i;
          }
        }
        return $scope.get('Opal')['$coerce_to!'](value, $scope.get('Integer'), "to_i");
      }

      if (base === undefined) {
        base = 0;
      } else {
        base = $scope.get('Opal').$coerce_to(base, $scope.get('Integer'), "to_int");
        if (base === 1 || base < 0 || base > 36) {
          self.$raise($scope.get('ArgumentError'), "invalid radix " + (base))
        }
      }

      str = value.toLowerCase();

      str = str.replace(/(\d)_(?=\d)/g, '$1');

      str = str.replace(/^(\s*[+-]?)(0[bodx]?)/, function (_, head, flag) {
        switch (flag) {
        case '0b':
          if (base === 0 || base === 2) {
            base = 2;
            return head;
          }
        case '0':
        case '0o':
          if (base === 0 || base === 8) {
            base = 8;
            return head;
          }
        case '0d':
          if (base === 0 || base === 10) {
            base = 10;
            return head;
          }
        case '0x':
          if (base === 0 || base === 16) {
            base = 16;
            return head;
          }
        }
        self.$raise($scope.get('ArgumentError'), "invalid value for Integer(): \"" + (value) + "\"")
      });

      base = (base === 0 ? 10 : base);

      base_digits = '0-' + (base <= 10 ? base - 1 : '9a-' + String.fromCharCode(97 + (base - 11)));

      if (!(new RegExp('^\\s*[+-]?[' + base_digits + ']+\\s*$')).test(str)) {
        self.$raise($scope.get('ArgumentError'), "invalid value for Integer(): \"" + (value) + "\"")
      }

      i = parseInt(str, base);

      if (isNaN(i)) {
        self.$raise($scope.get('ArgumentError'), "invalid value for Integer(): \"" + (value) + "\"")
      }

      return i;
    ;
    });

    Opal.defn(self, '$Float', function(value) {
      var self = this;

      
      var str;

      if (value === nil) {
        self.$raise($scope.get('TypeError'), "can't convert nil into Float")
      }

      if (value.$$is_string) {
        str = value.toString();

        str = str.replace(/(\d)_(?=\d)/g, '$1');

        //Special case for hex strings only:
        if (/^\s*[-+]?0[xX][0-9a-fA-F]+\s*$/.test(str)) {
          return self.$Integer(str);
        }

        if (!/^\s*[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?\s*$/.test(str)) {
          self.$raise($scope.get('ArgumentError'), "invalid value for Float(): \"" + (value) + "\"")
        }

        return parseFloat(str);
      }

      return $scope.get('Opal')['$coerce_to!'](value, $scope.get('Float'), "to_f");
    
    });

    Opal.defn(self, '$Hash', function(arg) {
      var $a, $b, self = this;

      if ((($a = ((($b = arg['$nil?']()) !== false && $b !== nil) ? $b : arg['$==']([]))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return $hash2([], {})};
      if ((($a = $scope.get('Hash')['$==='](arg)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return arg};
      return $scope.get('Opal')['$coerce_to!'](arg, $scope.get('Hash'), "to_hash");
    });

    Opal.defn(self, '$is_a?', function(klass) {
      var self = this;

      return Opal.is_a(self, klass);
    });

    Opal.defn(self, '$kind_of?', def['$is_a?']);

    Opal.defn(self, '$lambda', TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      block.$$is_lambda = true;
      return block;
    });

    Opal.defn(self, '$load', function(file) {
      var self = this;

      file = $scope.get('Opal')['$coerce_to!'](file, $scope.get('String'), "to_str");
      return Opal.load(Opal.normalize_loadable_path(file));
    });

    Opal.defn(self, '$loop', TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      
      while (true) {
        if (block() === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    });

    Opal.defn(self, '$nil?', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$object_id', def.$__id__);

    Opal.defn(self, '$printf', function(args) {
      var $a, self = this;

      args = $slice.call(arguments, 0);
      if ($rb_gt(args.$length(), 0)) {
        self.$print(($a = self).$format.apply($a, [].concat(args)))};
      return nil;
    });

    Opal.defn(self, '$private_methods', function() {
      var self = this;

      return [];
    });

    Opal.defn(self, '$private_instance_methods', def.$private_methods);

    Opal.defn(self, '$proc', TMP_7 = function() {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "tried to create Proc object without a block")
      };
      block.$$is_lambda = false;
      return block;
    });

    Opal.defn(self, '$puts', function(strs) {
      var $a, self = this;
      if ($gvars.stdout == null) $gvars.stdout = nil;

      strs = $slice.call(arguments, 0);
      return ($a = $gvars.stdout).$puts.apply($a, [].concat(strs));
    });

    Opal.defn(self, '$p', function(args) {
      var $a, $b, TMP_8, self = this;

      args = $slice.call(arguments, 0);
      ($a = ($b = args).$each, $a.$$p = (TMP_8 = function(obj){var self = TMP_8.$$s || this;
        if ($gvars.stdout == null) $gvars.stdout = nil;
if (obj == null) obj = nil;
      return $gvars.stdout.$puts(obj.$inspect())}, TMP_8.$$s = self, TMP_8), $a).call($b);
      if ($rb_le(args.$length(), 1)) {
        return args['$[]'](0)
        } else {
        return args
      };
    });

    Opal.defn(self, '$print', function(strs) {
      var $a, self = this;
      if ($gvars.stdout == null) $gvars.stdout = nil;

      strs = $slice.call(arguments, 0);
      return ($a = $gvars.stdout).$print.apply($a, [].concat(strs));
    });

    Opal.defn(self, '$warn', function(strs) {
      var $a, $b, self = this;
      if ($gvars.VERBOSE == null) $gvars.VERBOSE = nil;
      if ($gvars.stderr == null) $gvars.stderr = nil;

      strs = $slice.call(arguments, 0);
      if ((($a = ((($b = $gvars.VERBOSE['$nil?']()) !== false && $b !== nil) ? $b : strs['$empty?']())) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil
        } else {
        return ($a = $gvars.stderr).$puts.apply($a, [].concat(strs))
      };
    });

    Opal.defn(self, '$raise', function(exception, string) {
      var self = this;
      if ($gvars["!"] == null) $gvars["!"] = nil;

      
      if (exception == null && $gvars["!"]) {
        throw $gvars["!"];
      }

      if (exception == null) {
        exception = $scope.get('RuntimeError').$new();
      }
      else if (exception.$$is_string) {
        exception = $scope.get('RuntimeError').$new(exception);
      }
      else if (exception.$$is_class) {
        exception = exception.$new(string);
      }

      $gvars["!"] = exception;

      throw exception;
    ;
    });

    Opal.defn(self, '$fail', def.$raise);

    Opal.defn(self, '$rand', function(max) {
      var self = this;

      
      if (max === undefined) {
        return Math.random();
      }
      else if (max.$$is_range) {
        var arr = max.$to_a();

        return arr[self.$rand(arr.length)];
      }
      else {
        return Math.floor(Math.random() *
          Math.abs($scope.get('Opal').$coerce_to(max, $scope.get('Integer'), "to_int")));
      }
    
    });

    Opal.defn(self, '$respond_to?', function(name, include_all) {
      var $a, self = this;

      if (include_all == null) {
        include_all = false
      }
      if ((($a = self['$respond_to_missing?'](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      
      var body = self['$' + name];

      if (typeof(body) === "function" && !body.$$stub) {
        return true;
      }
    
      return false;
    });

    Opal.defn(self, '$respond_to_missing?', function(method_name) {
      var self = this;

      return false;
    });

    Opal.defn(self, '$require', function(file) {
      var self = this;

      file = $scope.get('Opal')['$coerce_to!'](file, $scope.get('String'), "to_str");
      return Opal.require(Opal.normalize_loadable_path(file));
    });

    Opal.defn(self, '$require_relative', function(file) {
      var self = this;

      $scope.get('Opal')['$try_convert!'](file, $scope.get('String'), "to_str");
      file = $scope.get('File').$expand_path($scope.get('File').$join(Opal.current_file, "..", file));
      return Opal.require(Opal.normalize_loadable_path(file));
    });

    Opal.defn(self, '$require_tree', function(path) {
      var self = this;

      path = $scope.get('File').$expand_path(path);
      if (path['$=='](".")) {
        path = ""};
      
      for (var name in Opal.modules) {
        if ((name)['$start_with?'](path)) {
          Opal.require(name);
        }
      }
    ;
      return nil;
    });

    Opal.defn(self, '$send', def.$__send__);

    Opal.defn(self, '$public_send', def.$__send__);

    Opal.defn(self, '$singleton_class', function() {
      var self = this;

      return Opal.get_singleton_class(self);
    });

    Opal.defn(self, '$sprintf', def.$format);

    Opal.defn(self, '$srand', def.$rand);

    Opal.defn(self, '$String', function(str) {
      var $a, self = this;

      return ((($a = $scope.get('Opal')['$coerce_to?'](str, $scope.get('String'), "to_str")) !== false && $a !== nil) ? $a : $scope.get('Opal')['$coerce_to!'](str, $scope.get('String'), "to_s"));
    });

    Opal.defn(self, '$taint', function() {
      var self = this;

      return self;
    });

    Opal.defn(self, '$tainted?', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$tap', TMP_9 = function() {
      var self = this, $iter = TMP_9.$$p, block = $iter || nil;

      TMP_9.$$p = null;
      if (Opal.yield1(block, self) === $breaker) return $breaker.$v;
      return self;
    });

    Opal.defn(self, '$to_proc', function() {
      var self = this;

      return self;
    });

    Opal.defn(self, '$to_s', function() {
      var self = this;

      return "#<" + (self.$class()) + ":0x" + (self.$__id__().$to_s(16)) + ">";
    });

    Opal.defn(self, '$untaint', def.$taint);
  })(self)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/nil_class"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$raise']);
  (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    def['$!'] = function() {
      var self = this;

      return true;
    };

    def['$&'] = function(other) {
      var self = this;

      return false;
    };

    def['$|'] = function(other) {
      var self = this;

      return other !== false && other !== nil;
    };

    def['$^'] = function(other) {
      var self = this;

      return other !== false && other !== nil;
    };

    def['$=='] = function(other) {
      var self = this;

      return other === nil;
    };

    def.$dup = function() {
      var self = this;

      return self.$raise($scope.get('TypeError'));
    };

    def.$inspect = function() {
      var self = this;

      return "nil";
    };

    def['$nil?'] = function() {
      var self = this;

      return true;
    };

    def.$singleton_class = function() {
      var self = this;

      return $scope.get('NilClass');
    };

    def.$to_a = function() {
      var self = this;

      return [];
    };

    def.$to_h = function() {
      var self = this;

      return Opal.hash();
    };

    def.$to_i = function() {
      var self = this;

      return 0;
    };

    Opal.defn(self, '$to_f', def.$to_i);

    return (def.$to_s = function() {
      var self = this;

      return "";
    }, nil) && 'to_s';
  })(self, null);
  return Opal.cdecl($scope, 'NIL', nil);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/boolean"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$undef_method']);
  (function($base, $super) {
    function $Boolean(){};
    var self = $Boolean = $klass($base, $super, 'Boolean', $Boolean);

    var def = self.$$proto, $scope = self.$$scope;

    def.$$is_boolean = true;

    def.$__id__ = function() {
      var self = this;

      return self.valueOf() ? 2 : 0;
    };

    Opal.defn(self, '$object_id', def.$__id__);

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      return self.$undef_method("new")
    })(self.$singleton_class());

    def['$!'] = function() {
      var self = this;

      return self != true;
    };

    def['$&'] = function(other) {
      var self = this;

      return (self == true) ? (other !== false && other !== nil) : false;
    };

    def['$|'] = function(other) {
      var self = this;

      return (self == true) ? true : (other !== false && other !== nil);
    };

    def['$^'] = function(other) {
      var self = this;

      return (self == true) ? (other === false || other === nil) : (other !== false && other !== nil);
    };

    def['$=='] = function(other) {
      var self = this;

      return (self == true) === other.valueOf();
    };

    Opal.defn(self, '$equal?', def['$==']);

    Opal.defn(self, '$singleton_class', def.$class);

    return (def.$to_s = function() {
      var self = this;

      return (self == true) ? 'true' : 'false';
    }, nil) && 'to_s';
  })(self, null);
  Opal.cdecl($scope, 'TrueClass', $scope.get('Boolean'));
  Opal.cdecl($scope, 'FalseClass', $scope.get('Boolean'));
  Opal.cdecl($scope, 'TRUE', true);
  return Opal.cdecl($scope, 'FALSE', false);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/error"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $module = Opal.module;

  Opal.add_stubs(['$attr_reader', '$class']);
  (function($base, $super) {
    function $Exception(){};
    var self = $Exception = $klass($base, $super, 'Exception', $Exception);

    var def = self.$$proto, $scope = self.$$scope;

    def.message = nil;
    self.$attr_reader("message");

    Opal.defs(self, '$new', function(message) {
      var self = this;

      if (message == null) {
        message = "Exception"
      }
      
      var err = new self.$$alloc(message);

      if (Error.captureStackTrace) {
        Error.captureStackTrace(err);
      }

      err.name = self.$$name;
      err.$initialize(message);
      return err;
    
    });

    def.$initialize = function(message) {
      var self = this;

      return self.message = message;
    };

    def.$backtrace = function() {
      var self = this;

      
      var backtrace = self.stack;

      if (typeof(backtrace) === 'string') {
        return backtrace.split("\n").slice(0, 15);
      }
      else if (backtrace) {
        return backtrace.slice(0, 15);
      }

      return [];
    
    };

    def.$inspect = function() {
      var self = this;

      return "#<" + (self.$class()) + ": '" + (self.message) + "'>";
    };

    return Opal.defn(self, '$to_s', def.$message);
  })(self, null);
  (function($base, $super) {
    function $ScriptError(){};
    var self = $ScriptError = $klass($base, $super, 'ScriptError', $ScriptError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $SyntaxError(){};
    var self = $SyntaxError = $klass($base, $super, 'SyntaxError', $SyntaxError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('ScriptError'));
  (function($base, $super) {
    function $LoadError(){};
    var self = $LoadError = $klass($base, $super, 'LoadError', $LoadError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('ScriptError'));
  (function($base, $super) {
    function $NotImplementedError(){};
    var self = $NotImplementedError = $klass($base, $super, 'NotImplementedError', $NotImplementedError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('ScriptError'));
  (function($base, $super) {
    function $SystemExit(){};
    var self = $SystemExit = $klass($base, $super, 'SystemExit', $SystemExit);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $NoMemoryError(){};
    var self = $NoMemoryError = $klass($base, $super, 'NoMemoryError', $NoMemoryError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $SignalException(){};
    var self = $SignalException = $klass($base, $super, 'SignalException', $SignalException);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $Interrupt(){};
    var self = $Interrupt = $klass($base, $super, 'Interrupt', $Interrupt);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $StandardError(){};
    var self = $StandardError = $klass($base, $super, 'StandardError', $StandardError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $NameError(){};
    var self = $NameError = $klass($base, $super, 'NameError', $NameError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $NoMethodError(){};
    var self = $NoMethodError = $klass($base, $super, 'NoMethodError', $NoMethodError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('NameError'));
  (function($base, $super) {
    function $RuntimeError(){};
    var self = $RuntimeError = $klass($base, $super, 'RuntimeError', $RuntimeError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $LocalJumpError(){};
    var self = $LocalJumpError = $klass($base, $super, 'LocalJumpError', $LocalJumpError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $TypeError(){};
    var self = $TypeError = $klass($base, $super, 'TypeError', $TypeError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $ArgumentError(){};
    var self = $ArgumentError = $klass($base, $super, 'ArgumentError', $ArgumentError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $IndexError(){};
    var self = $IndexError = $klass($base, $super, 'IndexError', $IndexError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $StopIteration(){};
    var self = $StopIteration = $klass($base, $super, 'StopIteration', $StopIteration);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('IndexError'));
  (function($base, $super) {
    function $KeyError(){};
    var self = $KeyError = $klass($base, $super, 'KeyError', $KeyError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('IndexError'));
  (function($base, $super) {
    function $RangeError(){};
    var self = $RangeError = $klass($base, $super, 'RangeError', $RangeError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $FloatDomainError(){};
    var self = $FloatDomainError = $klass($base, $super, 'FloatDomainError', $FloatDomainError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('RangeError'));
  (function($base, $super) {
    function $IOError(){};
    var self = $IOError = $klass($base, $super, 'IOError', $IOError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $SystemCallError(){};
    var self = $SystemCallError = $klass($base, $super, 'SystemCallError', $SystemCallError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  return (function($base) {
    var self = $module($base, 'Errno');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $EINVAL(){};
      var self = $EINVAL = $klass($base, $super, 'EINVAL', $EINVAL);

      var def = self.$$proto, $scope = self.$$scope, TMP_1;

      return (Opal.defs(self, '$new', TMP_1 = function() {
        var self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

        TMP_1.$$p = null;
        return Opal.find_super_dispatcher(self, 'new', TMP_1, null, $EINVAL).apply(self, ["Invalid argument"]);
      }), nil) && 'new'
    })(self, $scope.get('SystemCallError'))
  })(self);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/regexp"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$nil?', '$[]', '$raise', '$escape', '$options', '$to_str', '$new', '$join', '$!', '$match', '$begin', '$coerce_to', '$call', '$=~']);
  (function($base, $super) {
    function $RegexpError(){};
    var self = $RegexpError = $klass($base, $super, 'RegexpError', $RegexpError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  return (function($base, $super) {
    function $Regexp(){};
    var self = $Regexp = $klass($base, $super, 'Regexp', $Regexp);

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    Opal.cdecl($scope, 'IGNORECASE', 1);

    Opal.cdecl($scope, 'MULTILINE', 4);

    def.$$is_regexp = true;

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$escape = function(string) {
        var self = this;

        
        return string.replace(/([-[\]\/{}()*+?.^$\\| ])/g, '\\$1')
                     .replace(/[\n]/g, '\\n')
                     .replace(/[\r]/g, '\\r')
                     .replace(/[\f]/g, '\\f')
                     .replace(/[\t]/g, '\\t');
      
      };
      self.$$proto.$last_match = function(n) {
        var $a, self = this;
        if ($gvars["~"] == null) $gvars["~"] = nil;

        if (n == null) {
          n = nil
        }
        if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return $gvars["~"]
          } else {
          return $gvars["~"]['$[]'](n)
        };
      };
      self.$$proto.$quote = self.$$proto.$escape;
      self.$$proto.$union = function(parts) {
        var self = this;

        parts = $slice.call(arguments, 0);
        
        var is_first_part_array, quoted_validated, part, options, each_part_options;
        if (parts.length == 0) {
          return /(?!)/;
        }
        // cover the 2 arrays passed as arguments case
        is_first_part_array = parts[0].$$is_array;
        if (parts.length > 1 && is_first_part_array) {
          self.$raise($scope.get('TypeError'), "no implicit conversion of Array into String")
        }        
        // deal with splat issues (related to https://github.com/opal/opal/issues/858)
        if (is_first_part_array) {
          parts = parts[0];
        }
        options = undefined;
        quoted_validated = [];
        for (var i=0; i < parts.length; i++) {
          part = parts[i];
          if (part.$$is_string) {
            quoted_validated.push(self.$escape(part));
          }
          else if (part.$$is_regexp) { 
            each_part_options = (part).$options();   
            if (options != undefined && options != each_part_options) {
              self.$raise($scope.get('TypeError'), "All expressions must use the same options")
            }
            options = each_part_options;
            quoted_validated.push('('+part.source+')');
          }
          else {
            quoted_validated.push(self.$escape((part).$to_str()));
          }
        }
      
        return self.$new((quoted_validated).$join("|"), options);
      };
      return (self.$$proto.$new = function(regexp, options) {
        var self = this;

        
        // Play nice with IE8
        if (regexp.$$is_string && regexp.substr(regexp.length-1, 1) == "\\") {
          self.$raise($scope.get('RegexpError'), "too short escape sequence: /" + (regexp) + "/")
        }
        
        if (options == undefined || options['$!']()) {
          options = undefined;
        }
        
        if (options != undefined) {
          if (regexp.$$is_regexp) {
            // options are already in regex
            options = undefined;
          }
          else if (options.$$is_number) {
            var result = '';
            if ($scope.get('IGNORECASE') & options) {
              result += 'i';
            }
            if ($scope.get('MULTILINE') & options) {
              result += 'm';
            }
            options = result;
          }
          else {
            options = 'i';
          }
        }       
        
        return new RegExp(regexp, options);
      ;
      }, nil) && 'new';
    })(self.$singleton_class());

    def['$=='] = function(other) {
      var self = this;

      return other.constructor == RegExp && self.toString() === other.toString();
    };

    def['$==='] = function(string) {
      var self = this;

      return self.$match(string) !== nil;
    };

    def['$=~'] = function(string) {
      var $a, self = this;
      if ($gvars["~"] == null) $gvars["~"] = nil;

      return ($a = self.$match(string), $a !== false && $a !== nil ?$gvars["~"].$begin(0) : $a);
    };

    Opal.defn(self, '$eql?', def['$==']);

    def.$inspect = function() {
      var self = this;

      return self.toString();
    };

    def.$match = TMP_1 = function(string, pos) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;
      if ($gvars["~"] == null) $gvars["~"] = nil;

      TMP_1.$$p = null;
      
      if (pos === undefined) {
        pos = 0;
      } else {
        pos = $scope.get('Opal').$coerce_to(pos, $scope.get('Integer'), "to_int");
      }

      if (string === nil) {
        return $gvars["~"] = nil;
      }

      string = $scope.get('Opal').$coerce_to(string, $scope.get('String'), "to_str");

      if (pos < 0) {
        pos += string.length;
        if (pos < 0) {
          return $gvars["~"] = nil;
        }
      }

      // global RegExp maintains state, so not using self/this
      var md, re = new RegExp(self.source, 'gm' + (self.ignoreCase ? 'i' : ''));

      while (true) {
        md = re.exec(string);
        if (md === null) {
          return $gvars["~"] = nil;
        }
        if (md.index >= pos) {
          $gvars["~"] = $scope.get('MatchData').$new(re, md)
          return block === nil ? $gvars["~"] : block.$call($gvars["~"]);
        }
        re.lastIndex = md.index + 1;
      }
    ;
    };

    def['$~'] = function() {
      var self = this;
      if ($gvars._ == null) $gvars._ = nil;

      return self['$=~']($gvars._);
    };

    def.$source = function() {
      var self = this;

      return self.source;
    };

    def.$options = function() {
      var self = this;

      
      var as_string, text_flags, result, text_flag;
      as_string = self.toString();
      if (as_string == "/(?:)/") {
        self.$raise($scope.get('TypeError'), "uninitialized Regexp")
      }
      text_flags = as_string.replace(self.source, '').match(/\w+/);
      result = 0;
      // may have no flags
      if (text_flags == null) {
        return result;
      }
      // first match contains all of our flags
      text_flags = text_flags[0];
      for (var i=0; i < text_flags.length; i++) {
        text_flag = text_flags[i];
        switch(text_flag) {
          case 'i':
            result |= $scope.get('IGNORECASE');
            break;
          case 'm':
            result |= $scope.get('MULTILINE');
            break;
          default:
            self.$raise("RegExp flag " + (text_flag) + " does not have a match in Ruby")
        }
      }
      
      return result;
    
    };

    return Opal.defn(self, '$to_s', def.$source);
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/comparable"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_gt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs > rhs : lhs['$>'](rhs);
  }
  function $rb_lt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs < rhs : lhs['$<'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$===', '$equal?', '$<=>', '$normalize', '$raise', '$class']);
  return (function($base) {
    var self = $module($base, 'Comparable');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defs(self, '$normalize', function(what) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return what};
      if ($rb_gt(what, 0)) {
        return 1};
      if ($rb_lt(what, 0)) {
        return -1};
      return 0;
    });

    Opal.defn(self, '$==', function(other) {
      var $a, self = this, cmp = nil;

      try {
      if ((($a = self['$equal?'](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return true};
        if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          return false
        };
        return $scope.get('Comparable').$normalize(cmp) == 0;
      } catch ($err) {if (Opal.rescue($err, [$scope.get('StandardError')])) {
        return false
        }else { throw $err; }
      };
    });

    Opal.defn(self, '$>', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) > 0;
    });

    Opal.defn(self, '$>=', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) >= 0;
    });

    Opal.defn(self, '$<', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) < 0;
    });

    Opal.defn(self, '$<=', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) <= 0;
    });

    Opal.defn(self, '$between?', function(min, max) {
      var self = this;

      if ($rb_lt(self, min)) {
        return false};
      if ($rb_gt(self, max)) {
        return false};
      return true;
    });
  })(self)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/enumerable"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$raise', '$enum_for', '$flatten', '$map', '$==', '$destructure', '$nil?', '$coerce_to!', '$coerce_to', '$===', '$new', '$<<', '$[]', '$[]=', '$inspect', '$__send__', '$yield', '$enumerator_size', '$respond_to?', '$size', '$private', '$compare', '$<=>', '$dup', '$to_a', '$lambda', '$sort', '$call', '$first', '$zip']);
  return (function($base) {
    var self = $module($base, 'Enumerable');

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_7, TMP_8, TMP_9, TMP_10, TMP_11, TMP_12, TMP_13, TMP_14, TMP_15, TMP_16, TMP_17, TMP_18, TMP_19, TMP_20, TMP_22, TMP_23, TMP_24, TMP_25, TMP_26, TMP_27, TMP_28, TMP_29, TMP_30, TMP_31, TMP_32, TMP_33, TMP_35, TMP_37, TMP_41, TMP_42;

    Opal.defn(self, '$all?', TMP_1 = function() {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      
      var result = true;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
            result = false;
            return $breaker;
          }
        };
      }
      else {
        self.$each.$$p = function(obj) {
          if (arguments.length == 1 && (($a = obj) === nil || ($a.$$is_boolean && $a == false))) {
            result = false;
            return $breaker;
          }
        };
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$any?', TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      
      var result = false;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = true;
            return $breaker;
          }
        };
      }
      else {
        self.$each.$$p = function(obj) {
          if (arguments.length != 1 || (($a = obj) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = true;
            return $breaker;
          }
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$chunk', TMP_3 = function(state) {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$collect', TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var value = Opal.yieldX(block, arguments);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        result.push(value);
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$collect_concat', TMP_5 = function() {
      var $a, $b, TMP_6, self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect_concat")
      };
      return ($a = ($b = self).$map, $a.$$p = (TMP_6 = function(item){var self = TMP_6.$$s || this, $a;
if (item == null) item = nil;
      return $a = Opal.yield1(block, item), $a === $breaker ? $a : $a}, TMP_6.$$s = self, TMP_6), $a).call($b).$flatten(1);
    });

    Opal.defn(self, '$count', TMP_7 = function(object) {
      var $a, self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      var result = 0;

      if (object != null) {
        block = function() {
          return $scope.get('Opal').$destructure(arguments)['$=='](object);
        };
      }
      else if (block === nil) {
        block = function() { return true; };
      }

      self.$each.$$p = function() {
        var value = Opal.yieldX(block, arguments);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          result++;
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$cycle', TMP_8 = function(n) {
      var $a, self = this, $iter = TMP_8.$$p, block = $iter || nil;

      if (n == null) {
        n = nil
      }
      TMP_8.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("cycle", n)
      };
      if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        n = $scope.get('Opal')['$coerce_to!'](n, $scope.get('Integer'), "to_int");
        if ((($a = n <= 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil};
      };
      
      var result,
          all  = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        all.push(param);
      }

      self.$each();

      if (result !== undefined) {
        return result;
      }

      if (all.length === 0) {
        return nil;
      }
    
      if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        while (true) {
          for (var i = 0, length = all.length; i < length; i++) {
            var value = Opal.yield1(block, all[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }
        }
      
        } else {
        
        while (n > 1) {
          for (var i = 0, length = all.length; i < length; i++) {
            var value = Opal.yield1(block, all[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }

          n--;
        }
      
      };
    });

    Opal.defn(self, '$detect', TMP_9 = function(ifnone) {
      var $a, self = this, $iter = TMP_9.$$p, block = $iter || nil;

      TMP_9.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("detect", ifnone)
      };
      
      var result = undefined;

      self.$each.$$p = function() {
        var params = $scope.get('Opal').$destructure(arguments),
            value  = Opal.yield1(block, params);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          result = params;
          return $breaker;
        }
      };

      self.$each();

      if (result === undefined && ifnone !== undefined) {
        if (typeof(ifnone) === 'function') {
          result = ifnone();
        }
        else {
          result = ifnone;
        }
      }

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$drop', function(number) {
      var $a, self = this;

      number = $scope.get('Opal').$coerce_to(number, $scope.get('Integer'), "to_int");
      if ((($a = number < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "attempt to drop negative size")};
      
      var result  = [],
          current = 0;

      self.$each.$$p = function() {
        if (number <= current) {
          result.push($scope.get('Opal').$destructure(arguments));
        }

        current++;
      };

      self.$each()

      return result;
    
    });

    Opal.defn(self, '$drop_while', TMP_10 = function() {
      var $a, self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("drop_while")
      };
      
      var result   = [],
          dropping = true;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments);

        if (dropping) {
          var value = Opal.yield1(block, param);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
            dropping = false;
            result.push(param);
          }
        }
        else {
          result.push(param);
        }
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$each_cons', TMP_11 = function(n) {
      var self = this, $iter = TMP_11.$$p, block = $iter || nil;

      TMP_11.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$each_entry', TMP_12 = function() {
      var self = this, $iter = TMP_12.$$p, block = $iter || nil;

      TMP_12.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$each_slice', TMP_13 = function(n) {
      var $a, self = this, $iter = TMP_13.$$p, block = $iter || nil;

      TMP_13.$$p = null;
      n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
      if ((($a = n <= 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "invalid slice size")};
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_slice", n)
      };
      
      var result,
          slice = []

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments);

        slice.push(param);

        if (slice.length === n) {
          if (Opal.yield1(block, slice) === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          slice = [];
        }
      };

      self.$each();

      if (result !== undefined) {
        return result;
      }

      // our "last" group, if smaller than n then won't have been yielded
      if (slice.length > 0) {
        if (Opal.yield1(block, slice) === $breaker) {
          return $breaker.$v;
        }
      }
    ;
      return nil;
    });

    Opal.defn(self, '$each_with_index', TMP_14 = function(args) {
      var $a, self = this, $iter = TMP_14.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_14.$$p = null;
      if ((block !== nil)) {
        } else {
        return ($a = self).$enum_for.apply($a, ["each_with_index"].concat(args))
      };
      
      var result,
          index = 0;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = block(param, index);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        index++;
      };

      self.$each.apply(self, args);

      if (result !== undefined) {
        return result;
      }
    
      return self;
    });

    Opal.defn(self, '$each_with_object', TMP_15 = function(object) {
      var self = this, $iter = TMP_15.$$p, block = $iter || nil;

      TMP_15.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_with_object", object)
      };
      
      var result;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = block(param, object);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }
      };

      self.$each();

      if (result !== undefined) {
        return result;
      }
    
      return object;
    });

    Opal.defn(self, '$entries', function(args) {
      var self = this;

      args = $slice.call(arguments, 0);
      
      var result = [];

      self.$each.$$p = function() {
        result.push($scope.get('Opal').$destructure(arguments));
      };

      self.$each.apply(self, args);

      return result;
    
    });

    Opal.defn(self, '$find', def.$detect);

    Opal.defn(self, '$find_all', TMP_16 = function() {
      var $a, self = this, $iter = TMP_16.$$p, block = $iter || nil;

      TMP_16.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("find_all")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          result.push(param);
        }
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$find_index', TMP_17 = function(object) {
      var $a, self = this, $iter = TMP_17.$$p, block = $iter || nil;

      TMP_17.$$p = null;
      if ((($a = object === undefined && block === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$enum_for("find_index")};
      
      var result = nil,
          index  = 0;

      if (object != null) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if ((param)['$=='](object)) {
            result = index;
            return $breaker;
          }

          index += 1;
        };
      }
      else if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = index;
            return $breaker;
          }

          index += 1;
        };
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$first', function(number) {
      var $a, self = this, result = nil;

      if ((($a = number === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = nil;
        
        self.$each.$$p = function() {
          result = $scope.get('Opal').$destructure(arguments);

          return $breaker;
        };

        self.$each();
      ;
        } else {
        result = [];
        number = $scope.get('Opal').$coerce_to(number, $scope.get('Integer'), "to_int");
        if ((($a = number < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "attempt to take negative size")};
        if ((($a = number == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return []};
        
        var current = 0,
            number  = $scope.get('Opal').$coerce_to(number, $scope.get('Integer'), "to_int");

        self.$each.$$p = function() {
          result.push($scope.get('Opal').$destructure(arguments));

          if (number <= ++current) {
            return $breaker;
          }
        };

        self.$each();
      ;
      };
      return result;
    });

    Opal.defn(self, '$flat_map', def.$collect_concat);

    Opal.defn(self, '$grep', TMP_18 = function(pattern) {
      var $a, self = this, $iter = TMP_18.$$p, block = $iter || nil;

      TMP_18.$$p = null;
      
      var result = [];

      if (block !== nil) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments),
              value = pattern['$==='](param);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            value = Opal.yield1(block, param);

            if (value === $breaker) {
              result = $breaker.$v;
              return $breaker;
            }

            result.push(value);
          }
        };
      }
      else {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments),
              value = pattern['$==='](param);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result.push(param);
          }
        };
      }

      self.$each();

      return result;
    ;
    });

    Opal.defn(self, '$group_by', TMP_19 = function() {
      var $a, $b, $c, self = this, $iter = TMP_19.$$p, block = $iter || nil, hash = nil;

      TMP_19.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("group_by")
      };
      hash = $scope.get('Hash').$new();
      
      var result;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        (($a = value, $b = hash, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, []))))['$<<'](param);
      }

      self.$each();

      if (result !== undefined) {
        return result;
      }
    
      return hash;
    });

    Opal.defn(self, '$include?', function(obj) {
      var self = this;

      
      var result = false;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments);

        if ((param)['$=='](obj)) {
          result = true;
          return $breaker;
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$inject', TMP_20 = function(object, sym) {
      var self = this, $iter = TMP_20.$$p, block = $iter || nil;

      TMP_20.$$p = null;
      
      var result = object;

      if (block !== nil && sym === undefined) {
        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = value;
            return;
          }

          value = Opal.yieldX(block, [result, value]);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          result = value;
        };
      }
      else {
        if (sym === undefined) {
          if (!$scope.get('Symbol')['$==='](object)) {
            self.$raise($scope.get('TypeError'), "" + (object.$inspect()) + " is not a Symbol");
          }

          sym    = object;
          result = undefined;
        }

        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = value;
            return;
          }

          result = (result).$__send__(sym, value);
        };
      }

      self.$each();

      return result == undefined ? nil : result;
    ;
    });

    Opal.defn(self, '$lazy', function() {
      var $a, $b, TMP_21, self = this;

      return ($a = ($b = (($scope.get('Enumerator')).$$scope.get('Lazy'))).$new, $a.$$p = (TMP_21 = function(enum$, args){var self = TMP_21.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
      return ($a = enum$).$yield.apply($a, [].concat(args))}, TMP_21.$$s = self, TMP_21), $a).call($b, self, self.$enumerator_size());
    });

    Opal.defn(self, '$enumerator_size', function() {
      var $a, self = this;

      if ((($a = self['$respond_to?']("size")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$size()
        } else {
        return nil
      };
    });

    self.$private("enumerator_size");

    Opal.defn(self, '$map', def.$collect);

    Opal.defn(self, '$max', TMP_22 = function() {
      var self = this, $iter = TMP_22.$$p, block = $iter || nil;

      TMP_22.$$p = null;
      
      var result;

      if (block !== nil) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          var value = block(param, result);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if (value === nil) {
            self.$raise($scope.get('ArgumentError'), "comparison failed");
          }

          if (value > 0) {
            result = param;
          }
        };
      }
      else {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          if ($scope.get('Opal').$compare(param, result) > 0) {
            result = param;
          }
        };
      }

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$max_by', TMP_23 = function() {
      var self = this, $iter = TMP_23.$$p, block = $iter || nil;

      TMP_23.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("max_by")
      };
      
      var result,
          by;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (result === undefined) {
          result = param;
          by     = value;
          return;
        }

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((value)['$<=>'](by) > 0) {
          result = param
          by     = value;
        }
      };

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$member?', def['$include?']);

    Opal.defn(self, '$min', TMP_24 = function() {
      var self = this, $iter = TMP_24.$$p, block = $iter || nil;

      TMP_24.$$p = null;
      
      var result;

      if (block !== nil) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          var value = block(param, result);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if (value === nil) {
            self.$raise($scope.get('ArgumentError'), "comparison failed");
          }

          if (value < 0) {
            result = param;
          }
        };
      }
      else {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          if ($scope.get('Opal').$compare(param, result) < 0) {
            result = param;
          }
        };
      }

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$min_by', TMP_25 = function() {
      var self = this, $iter = TMP_25.$$p, block = $iter || nil;

      TMP_25.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("min_by")
      };
      
      var result,
          by;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (result === undefined) {
          result = param;
          by     = value;
          return;
        }

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((value)['$<=>'](by) < 0) {
          result = param
          by     = value;
        }
      };

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$minmax', TMP_26 = function() {
      var self = this, $iter = TMP_26.$$p, block = $iter || nil;

      TMP_26.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$minmax_by', TMP_27 = function() {
      var self = this, $iter = TMP_27.$$p, block = $iter || nil;

      TMP_27.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$none?', TMP_28 = function() {
      var $a, self = this, $iter = TMP_28.$$p, block = $iter || nil;

      TMP_28.$$p = null;
      
      var result = true;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = false;
            return $breaker;
          }
        }
      }
      else {
        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = false;
            return $breaker;
          }
        };
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$one?', TMP_29 = function() {
      var $a, self = this, $iter = TMP_29.$$p, block = $iter || nil;

      TMP_29.$$p = null;
      
      var result = false;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            if (result === true) {
              result = false;
              return $breaker;
            }

            result = true;
          }
        }
      }
      else {
        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            if (result === true) {
              result = false;
              return $breaker;
            }

            result = true;
          }
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$partition', TMP_30 = function() {
      var $a, self = this, $iter = TMP_30.$$p, block = $iter || nil;

      TMP_30.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("partition")
      };
      
      var truthy = [], falsy = [], result;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          truthy.push(param);
        }
        else {
          falsy.push(param);
        }
      };

      self.$each();

      return [truthy, falsy];
    
    });

    Opal.defn(self, '$reduce', def.$inject);

    Opal.defn(self, '$reject', TMP_31 = function() {
      var $a, self = this, $iter = TMP_31.$$p, block = $iter || nil;

      TMP_31.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reject")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
          result.push(param);
        }
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$reverse_each', TMP_32 = function() {
      var self = this, $iter = TMP_32.$$p, block = $iter || nil;

      TMP_32.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reverse_each")
      };
      
      var result = [];

      self.$each.$$p = function() {
        result.push(arguments);
      };

      self.$each();

      for (var i = result.length - 1; i >= 0; i--) {
        Opal.yieldX(block, result[i]);
      }

      return result;
    
    });

    Opal.defn(self, '$select', def.$find_all);

    Opal.defn(self, '$slice_before', TMP_33 = function(pattern) {
      var $a, $b, TMP_34, self = this, $iter = TMP_33.$$p, block = $iter || nil;

      TMP_33.$$p = null;
      if ((($a = pattern === undefined && block === nil || arguments.length > 1) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (arguments.length) + " for 1)")};
      return ($a = ($b = $scope.get('Enumerator')).$new, $a.$$p = (TMP_34 = function(e){var self = TMP_34.$$s || this, $a;
if (e == null) e = nil;
      
        var slice = [];

        if (block !== nil) {
          if (pattern === undefined) {
            self.$each.$$p = function() {
              var param = $scope.get('Opal').$destructure(arguments),
                  value = Opal.yield1(block, param);

              if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true)) && slice.length > 0) {
                e['$<<'](slice);
                slice = [];
              }

              slice.push(param);
            };
          }
          else {
            self.$each.$$p = function() {
              var param = $scope.get('Opal').$destructure(arguments),
                  value = block(param, pattern.$dup());

              if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true)) && slice.length > 0) {
                e['$<<'](slice);
                slice = [];
              }

              slice.push(param);
            };
          }
        }
        else {
          self.$each.$$p = function() {
            var param = $scope.get('Opal').$destructure(arguments),
                value = pattern['$==='](param);

            if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true)) && slice.length > 0) {
              e['$<<'](slice);
              slice = [];
            }

            slice.push(param);
          };
        }

        self.$each();

        if (slice.length > 0) {
          e['$<<'](slice);
        }
      ;}, TMP_34.$$s = self, TMP_34), $a).call($b);
    });

    Opal.defn(self, '$sort', TMP_35 = function() {
      var $a, $b, TMP_36, self = this, $iter = TMP_35.$$p, block = $iter || nil, ary = nil;

      TMP_35.$$p = null;
      ary = self.$to_a();
      if ((block !== nil)) {
        } else {
        block = ($a = ($b = self).$lambda, $a.$$p = (TMP_36 = function(a, b){var self = TMP_36.$$s || this;
if (a == null) a = nil;if (b == null) b = nil;
        return a['$<=>'](b)}, TMP_36.$$s = self, TMP_36), $a).call($b)
      };
      return ary.sort(block);
    });

    Opal.defn(self, '$sort_by', TMP_37 = function() {
      var $a, $b, TMP_38, $c, $d, TMP_39, $e, $f, TMP_40, self = this, $iter = TMP_37.$$p, block = $iter || nil;

      TMP_37.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("sort_by")
      };
      return ($a = ($b = ($c = ($d = ($e = ($f = self).$map, $e.$$p = (TMP_40 = function(){var self = TMP_40.$$s || this, arg = nil;

      arg = $scope.get('Opal').$destructure(arguments);
        return [block.$call(arg), arg];}, TMP_40.$$s = self, TMP_40), $e).call($f)).$sort, $c.$$p = (TMP_39 = function(a, b){var self = TMP_39.$$s || this;
if (a == null) a = nil;if (b == null) b = nil;
      return a['$[]'](0)['$<=>'](b['$[]'](0))}, TMP_39.$$s = self, TMP_39), $c).call($d)).$map, $a.$$p = (TMP_38 = function(arg){var self = TMP_38.$$s || this;
if (arg == null) arg = nil;
      return arg[1];}, TMP_38.$$s = self, TMP_38), $a).call($b);
    });

    Opal.defn(self, '$take', function(num) {
      var self = this;

      return self.$first(num);
    });

    Opal.defn(self, '$take_while', TMP_41 = function() {
      var $a, self = this, $iter = TMP_41.$$p, block = $iter || nil;

      TMP_41.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("take_while")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
          return $breaker;
        }

        result.push(param);
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$to_a', def.$entries);

    Opal.defn(self, '$zip', TMP_42 = function(others) {
      var $a, self = this, $iter = TMP_42.$$p, block = $iter || nil;

      others = $slice.call(arguments, 0);
      TMP_42.$$p = null;
      return ($a = self.$to_a()).$zip.apply($a, [].concat(others));
    });
  })(self)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/enumerator"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  function $rb_lt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs < rhs : lhs['$<'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$allocate', '$new', '$to_proc', '$coerce_to', '$nil?', '$empty?', '$class', '$__send__', '$===', '$call', '$enum_for', '$destructure', '$inspect', '$[]', '$raise', '$yield', '$each', '$enumerator_size', '$respond_to?', '$try_convert', '$for']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Enumerator(){};
    var self = $Enumerator = $klass($base, $super, 'Enumerator', $Enumerator);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    def.size = def.args = def.object = def.method = nil;
    self.$include($scope.get('Enumerable'));

    Opal.defs(self, '$for', TMP_1 = function(object, method, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 2);
      if (method == null) {
        method = "each"
      }
      TMP_1.$$p = null;
      
      var obj = self.$allocate();

      obj.object = object;
      obj.size   = block;
      obj.method = method;
      obj.args   = args;

      return obj;
    ;
    });

    def.$initialize = TMP_2 = function() {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if (block !== false && block !== nil) {
        self.object = ($a = ($b = $scope.get('Generator')).$new, $a.$$p = block.$to_proc(), $a).call($b);
        self.method = "each";
        self.args = [];
        self.size = arguments[0] || nil;
        if ((($a = self.size) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self.size = $scope.get('Opal').$coerce_to(self.size, $scope.get('Integer'), "to_int")
          } else {
          return nil
        };
        } else {
        self.object = arguments[0];
        self.method = arguments[1] || "each";
        self.args = $slice.call(arguments, 2);
        return self.size = nil;
      };
    };

    def.$each = TMP_3 = function(args) {
      var $a, $b, $c, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      if ((($a = ($b = block['$nil?'](), $b !== false && $b !== nil ?args['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      args = $rb_plus(self.args, args);
      if ((($a = block['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = self.$class()).$new.apply($a, [self.object, self.method].concat(args))};
      return ($b = ($c = self.object).$__send__, $b.$$p = block.$to_proc(), $b).apply($c, [self.method].concat(args));
    };

    def.$size = function() {
      var $a, self = this;

      if ((($a = $scope.get('Proc')['$==='](self.size)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = self.size).$call.apply($a, [].concat(self.args))
        } else {
        return self.size
      };
    };

    def.$with_index = TMP_4 = function(offset) {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      if (offset == null) {
        offset = 0
      }
      TMP_4.$$p = null;
      if (offset !== false && offset !== nil) {
        offset = $scope.get('Opal').$coerce_to(offset, $scope.get('Integer'), "to_int")
        } else {
        offset = 0
      };
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("with_index", offset)
      };
      
      var result, index = offset;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = block(param, index);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        index++;
      }

      self.$each();

      if (result !== undefined) {
        return result;
      }

      return self.object;
    
    };

    Opal.defn(self, '$with_object', def.$each_with_object);

    def.$inspect = function() {
      var $a, self = this, result = nil;

      result = "#<" + (self.$class()) + ": " + (self.object.$inspect()) + ":" + (self.method);
      if ((($a = self.args['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        result = $rb_plus(result, "(" + (self.args.$inspect()['$[]']($scope.get('Range').$new(1, -2))) + ")")
      };
      return $rb_plus(result, ">");
    };

    (function($base, $super) {
      function $Generator(){};
      var self = $Generator = $klass($base, $super, 'Generator', $Generator);

      var def = self.$$proto, $scope = self.$$scope, TMP_5, TMP_6;

      def.block = nil;
      self.$include($scope.get('Enumerable'));

      def.$initialize = TMP_5 = function() {
        var self = this, $iter = TMP_5.$$p, block = $iter || nil;

        TMP_5.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('LocalJumpError'), "no block given")
        };
        return self.block = block;
      };

      return (def.$each = TMP_6 = function(args) {
        var $a, $b, self = this, $iter = TMP_6.$$p, block = $iter || nil, yielder = nil;

        args = $slice.call(arguments, 0);
        TMP_6.$$p = null;
        yielder = ($a = ($b = $scope.get('Yielder')).$new, $a.$$p = block.$to_proc(), $a).call($b);
        
        try {
          args.unshift(yielder);

          if (Opal.yieldX(self.block, args) === $breaker) {
            return $breaker.$v;
          }
        }
        catch (e) {
          if (e === $breaker) {
            return $breaker.$v;
          }
          else {
            throw e;
          }
        }
      ;
        return self;
      }, nil) && 'each';
    })(self, null);

    (function($base, $super) {
      function $Yielder(){};
      var self = $Yielder = $klass($base, $super, 'Yielder', $Yielder);

      var def = self.$$proto, $scope = self.$$scope, TMP_7;

      def.block = nil;
      def.$initialize = TMP_7 = function() {
        var self = this, $iter = TMP_7.$$p, block = $iter || nil;

        TMP_7.$$p = null;
        return self.block = block;
      };

      def.$yield = function(values) {
        var self = this;

        values = $slice.call(arguments, 0);
        
        var value = Opal.yieldX(self.block, values);

        if (value === $breaker) {
          throw $breaker;
        }

        return value;
      ;
      };

      return (def['$<<'] = function(values) {
        var $a, self = this;

        values = $slice.call(arguments, 0);
        ($a = self).$yield.apply($a, [].concat(values));
        return self;
      }, nil) && '<<';
    })(self, null);

    return (function($base, $super) {
      function $Lazy(){};
      var self = $Lazy = $klass($base, $super, 'Lazy', $Lazy);

      var def = self.$$proto, $scope = self.$$scope, TMP_8, TMP_11, TMP_13, TMP_18, TMP_20, TMP_21, TMP_23, TMP_26, TMP_29;

      def.enumerator = nil;
      (function($base, $super) {
        function $StopLazyError(){};
        var self = $StopLazyError = $klass($base, $super, 'StopLazyError', $StopLazyError);

        var def = self.$$proto, $scope = self.$$scope;

        return nil;
      })(self, $scope.get('Exception'));

      def.$initialize = TMP_8 = function(object, size) {
        var TMP_9, self = this, $iter = TMP_8.$$p, block = $iter || nil;

        if (size == null) {
          size = nil
        }
        TMP_8.$$p = null;
        if ((block !== nil)) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy new without a block")
        };
        self.enumerator = object;
        return Opal.find_super_dispatcher(self, 'initialize', TMP_8, (TMP_9 = function(yielder, each_args){var self = TMP_9.$$s || this, $a, $b, TMP_10;
if (yielder == null) yielder = nil;each_args = $slice.call(arguments, 1);
        try {
          return ($a = ($b = object).$each, $a.$$p = (TMP_10 = function(args){var self = TMP_10.$$s || this;
args = $slice.call(arguments, 0);
            
              args.unshift(yielder);

              if (Opal.yieldX(block, args) === $breaker) {
                return $breaker;
              }
            ;}, TMP_10.$$s = self, TMP_10), $a).apply($b, [].concat(each_args))
          } catch ($err) {if (Opal.rescue($err, [$scope.get('Exception')])) {
            return nil
            }else { throw $err; }
          }}, TMP_9.$$s = self, TMP_9)).apply(self, [size]);
      };

      Opal.defn(self, '$force', def.$to_a);

      def.$lazy = function() {
        var self = this;

        return self;
      };

      def.$collect = TMP_11 = function() {
        var $a, $b, TMP_12, self = this, $iter = TMP_11.$$p, block = $iter || nil;

        TMP_11.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy map without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_12 = function(enum$, args){var self = TMP_12.$$s || this;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          enum$.$yield(value);
        }, TMP_12.$$s = self, TMP_12), $a).call($b, self, self.$enumerator_size());
      };

      def.$collect_concat = TMP_13 = function() {
        var $a, $b, TMP_14, self = this, $iter = TMP_13.$$p, block = $iter || nil;

        TMP_13.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy map without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_14 = function(enum$, args){var self = TMP_14.$$s || this, $a, $b, TMP_15, $c, TMP_16;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((value)['$respond_to?']("force") && (value)['$respond_to?']("each")) {
            ($a = ($b = (value)).$each, $a.$$p = (TMP_15 = function(v){var self = TMP_15.$$s || this;
if (v == null) v = nil;
          return enum$.$yield(v)}, TMP_15.$$s = self, TMP_15), $a).call($b)
          }
          else {
            var array = $scope.get('Opal').$try_convert(value, $scope.get('Array'), "to_ary");

            if (array === nil) {
              enum$.$yield(value);
            }
            else {
              ($a = ($c = (value)).$each, $a.$$p = (TMP_16 = function(v){var self = TMP_16.$$s || this;
if (v == null) v = nil;
          return enum$.$yield(v)}, TMP_16.$$s = self, TMP_16), $a).call($c);
            }
          }
        ;}, TMP_14.$$s = self, TMP_14), $a).call($b, self, nil);
      };

      def.$drop = function(n) {
        var $a, $b, TMP_17, self = this, current_size = nil, set_size = nil, dropped = nil;

        n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
        if ($rb_lt(n, 0)) {
          self.$raise($scope.get('ArgumentError'), "attempt to drop negative size")};
        current_size = self.$enumerator_size();
        set_size = (function() {if ((($a = $scope.get('Integer')['$==='](current_size)) !== nil && (!$a.$$is_boolean || $a == true))) {
          if ($rb_lt(n, current_size)) {
            return n
            } else {
            return current_size
          }
          } else {
          return current_size
        }; return nil; })();
        dropped = 0;
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_17 = function(enum$, args){var self = TMP_17.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        if ($rb_lt(dropped, n)) {
            return dropped = $rb_plus(dropped, 1)
            } else {
            return ($a = enum$).$yield.apply($a, [].concat(args))
          }}, TMP_17.$$s = self, TMP_17), $a).call($b, self, set_size);
      };

      def.$drop_while = TMP_18 = function() {
        var $a, $b, TMP_19, self = this, $iter = TMP_18.$$p, block = $iter || nil, succeeding = nil;

        TMP_18.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy drop_while without a block")
        };
        succeeding = true;
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_19 = function(enum$, args){var self = TMP_19.$$s || this, $a, $b;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        if (succeeding !== false && succeeding !== nil) {
            
            var value = Opal.yieldX(block, args);

            if (value === $breaker) {
              return $breaker;
            }

            if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
              succeeding = false;

              ($a = enum$).$yield.apply($a, [].concat(args));
            }
          
            } else {
            return ($b = enum$).$yield.apply($b, [].concat(args))
          }}, TMP_19.$$s = self, TMP_19), $a).call($b, self, nil);
      };

      def.$enum_for = TMP_20 = function(method, args) {
        var $a, $b, self = this, $iter = TMP_20.$$p, block = $iter || nil;

        args = $slice.call(arguments, 1);
        if (method == null) {
          method = "each"
        }
        TMP_20.$$p = null;
        return ($a = ($b = self.$class()).$for, $a.$$p = block.$to_proc(), $a).apply($b, [self, method].concat(args));
      };

      def.$find_all = TMP_21 = function() {
        var $a, $b, TMP_22, self = this, $iter = TMP_21.$$p, block = $iter || nil;

        TMP_21.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy select without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_22 = function(enum$, args){var self = TMP_22.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = enum$).$yield.apply($a, [].concat(args));
          }
        ;}, TMP_22.$$s = self, TMP_22), $a).call($b, self, nil);
      };

      Opal.defn(self, '$flat_map', def.$collect_concat);

      def.$grep = TMP_23 = function(pattern) {
        var $a, $b, TMP_24, $c, TMP_25, self = this, $iter = TMP_23.$$p, block = $iter || nil;

        TMP_23.$$p = null;
        if (block !== false && block !== nil) {
          return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_24 = function(enum$, args){var self = TMP_24.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
          
            var param = $scope.get('Opal').$destructure(args),
                value = pattern['$==='](param);

            if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
              value = Opal.yield1(block, param);

              if (value === $breaker) {
                return $breaker;
              }

              enum$.$yield(Opal.yield1(block, param));
            }
          ;}, TMP_24.$$s = self, TMP_24), $a).call($b, self, nil)
          } else {
          return ($a = ($c = $scope.get('Lazy')).$new, $a.$$p = (TMP_25 = function(enum$, args){var self = TMP_25.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
          
            var param = $scope.get('Opal').$destructure(args),
                value = pattern['$==='](param);

            if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
              enum$.$yield(param);
            }
          ;}, TMP_25.$$s = self, TMP_25), $a).call($c, self, nil)
        };
      };

      Opal.defn(self, '$map', def.$collect);

      Opal.defn(self, '$select', def.$find_all);

      def.$reject = TMP_26 = function() {
        var $a, $b, TMP_27, self = this, $iter = TMP_26.$$p, block = $iter || nil;

        TMP_26.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy reject without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_27 = function(enum$, args){var self = TMP_27.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
            ($a = enum$).$yield.apply($a, [].concat(args));
          }
        ;}, TMP_27.$$s = self, TMP_27), $a).call($b, self, nil);
      };

      def.$take = function(n) {
        var $a, $b, TMP_28, self = this, current_size = nil, set_size = nil, taken = nil;

        n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
        if ($rb_lt(n, 0)) {
          self.$raise($scope.get('ArgumentError'), "attempt to take negative size")};
        current_size = self.$enumerator_size();
        set_size = (function() {if ((($a = $scope.get('Integer')['$==='](current_size)) !== nil && (!$a.$$is_boolean || $a == true))) {
          if ($rb_lt(n, current_size)) {
            return n
            } else {
            return current_size
          }
          } else {
          return current_size
        }; return nil; })();
        taken = 0;
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_28 = function(enum$, args){var self = TMP_28.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        if ($rb_lt(taken, n)) {
            ($a = enum$).$yield.apply($a, [].concat(args));
            return taken = $rb_plus(taken, 1);
            } else {
            return self.$raise($scope.get('StopLazyError'))
          }}, TMP_28.$$s = self, TMP_28), $a).call($b, self, set_size);
      };

      def.$take_while = TMP_29 = function() {
        var $a, $b, TMP_30, self = this, $iter = TMP_29.$$p, block = $iter || nil;

        TMP_29.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy take_while without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_30 = function(enum$, args){var self = TMP_30.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = enum$).$yield.apply($a, [].concat(args));
          }
          else {
            self.$raise($scope.get('StopLazyError'));
          }
        ;}, TMP_30.$$s = self, TMP_30), $a).call($b, self, nil);
      };

      Opal.defn(self, '$to_enum', def.$enum_for);

      return (def.$inspect = function() {
        var self = this;

        return "#<" + (self.$class()) + ": " + (self.enumerator.$inspect()) + ">";
      }, nil) && 'inspect';
    })(self, self);
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/array"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_gt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs > rhs : lhs['$>'](rhs);
  }
  function $rb_lt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs < rhs : lhs['$<'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars, $range = Opal.range, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$include', '$new', '$class', '$raise', '$===', '$to_a', '$respond_to?', '$to_ary', '$coerce_to', '$coerce_to?', '$==', '$to_str', '$clone', '$hash', '$<=>', '$object_id', '$inspect', '$enum_for', '$empty?', '$nil?', '$coerce_to!', '$initialize_clone', '$initialize_dup', '$replace', '$eql?', '$length', '$begin', '$end', '$exclude_end?', '$flatten', '$__id__', '$[]', '$to_s', '$join', '$delete_if', '$to_proc', '$each', '$reverse', '$frozen?', '$rotate', '$!', '$map', '$rand', '$keep_if', '$shuffle!', '$sort', '$times', '$[]=', '$<<', '$at', '$kind_of?', '$last', '$first', '$upto']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_8, TMP_9, TMP_10, TMP_11, TMP_12, TMP_13, TMP_14, TMP_15, TMP_16, TMP_17, TMP_18, TMP_19, TMP_21, TMP_22, TMP_23, TMP_24, TMP_25, TMP_30;

    def.length = nil;
    self.$include($scope.get('Enumerable'));

    def.$$is_array = true;

    Opal.defs(self, '$[]', function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      return objects;
    });

    def.$initialize = function(args) {
      var $a, self = this;

      args = $slice.call(arguments, 0);
      return ($a = self.$class()).$new.apply($a, [].concat(args));
    };

    Opal.defs(self, '$new', TMP_1 = function(size, obj) {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      if (size == null) {
        size = nil
      }
      if (obj == null) {
        obj = nil
      }
      TMP_1.$$p = null;
      if ((($a = arguments.length > 2) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (arguments.length) + " for 0..2)")};
      if ((($a = arguments.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if ((($a = arguments.length === 1) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = $scope.get('Array')['$==='](size)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return size.$to_a()
        } else if ((($a = size['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return size.$to_ary()}};
      size = $scope.get('Opal').$coerce_to(size, $scope.get('Integer'), "to_int");
      if ((($a = size < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative array size")};
      
      var result = [];

      if (block === nil) {
        for (var i = 0; i < size; i++) {
          result.push(obj);
        }
      }
      else {
        for (var i = 0, value; i < size; i++) {
          value = block(i);

          if (value === $breaker) {
            return $breaker.$v;
          }

          result[i] = value;
        }
      }

      return result;
    
    });

    Opal.defs(self, '$try_convert', function(obj) {
      var self = this;

      return $scope.get('Opal')['$coerce_to?'](obj, $scope.get('Array'), "to_ary");
    });

    def['$&'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      var result = [],
          seen   = {};

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if (!seen[item]) {
          for (var j = 0, length2 = other.length; j < length2; j++) {
            var item2 = other[j];

            if (!seen[item2] && (item)['$=='](item2)) {
              seen[item] = true;
              result.push(item);
            }
          }
        }
      }

      return result;
    
    };

    def['$|'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      var result = [],
          seen   = {};

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if (!seen[item]) {
          seen[item] = true;
          result.push(item);
        }
      }

      for (var i = 0, length = other.length; i < length; i++) {
        var item = other[i];

        if (!seen[item]) {
          seen[item] = true;
          result.push(item);
        }
      }
      return result;
    
    };

    def['$*'] = function(other) {
      var $a, self = this;

      if ((($a = other['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.join(other.$to_str())};
      if ((($a = other['$respond_to?']("to_int")) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "no implicit conversion of " + (other.$class()) + " into Integer")
      };
      other = $scope.get('Opal').$coerce_to(other, $scope.get('Integer'), "to_int");
      if ((($a = other < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative argument")};
      
      var result = [];

      for (var i = 0; i < other; i++) {
        result = result.concat(self);
      }

      return result;
    
    };

    def['$+'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      return self.concat(other);
    };

    def['$-'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if ((($a = other.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$clone()};
      
      var seen   = {},
          result = [];

      for (var i = 0, length = other.length; i < length; i++) {
        seen[other[i]] = true;
      }

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if (!seen[item]) {
          result.push(item);
        }
      }

      return result;
    
    };

    def['$<<'] = function(object) {
      var self = this;

      self.push(object);
      return self;
    };

    def['$<=>'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
      } else if ((($a = other['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_ary().$to_a()
        } else {
        return nil
      };
      
      if (self.$hash() === other.$hash()) {
        return 0;
      }

      if (self.length != other.length) {
        return (self.length > other.length) ? 1 : -1;
      }

      for (var i = 0, length = self.length; i < length; i++) {
        var tmp = (self[i])['$<=>'](other[i]);

        if (tmp !== 0) {
          return tmp;
        }
      }

      return 0;
    ;
    };

    def['$=='] = function(other) {
      var self = this;

      
      var recursed = {};

      function _eqeq(array, other) {
        var i, length, a, b;

        if (!other.$$is_array) {
          if ($scope.get('Opal')['$respond_to?'](other, "to_ary")) {
            return (other)['$=='](array);
          } else {
            return false;
          }
        }

        other = other.$to_a();

        if (array.length !== other.length) {
          return false;
        }

        recursed[(array).$object_id()] = true;

        for (i = 0, length = array.length; i < length; i++) {
          a = array[i];
          b = other[i];
          if (a.$$is_array) {
            if (b.$$is_array && b.length !== a.length) {
              return false;
            }
            if (!recursed.hasOwnProperty((a).$object_id())) {
              if (!_eqeq(a, b)) {
                return false;
              }
            }
          } else {
            if (!(a)['$=='](b)) {
              return false;
            }
          }
        }

        return true;
      }

      return _eqeq(self, other);
    ;
    };

    def['$[]'] = function(index, length) {
      var $a, self = this;

      if ((($a = $scope.get('Range')['$==='](index)) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        var size    = self.length,
            exclude = index.exclude,
            from    = $scope.get('Opal').$coerce_to(index.begin, $scope.get('Integer'), "to_int"),
            to      = $scope.get('Opal').$coerce_to(index.end, $scope.get('Integer'), "to_int");

        if (from < 0) {
          from += size;

          if (from < 0) {
            return nil;
          }
        }

        if (from > size) {
          return nil;
        }

        if (to < 0) {
          to += size;

          if (to < 0) {
            return [];
          }
        }

        if (!exclude) {
          to += 1;
        }

        return self.slice(from, to);
      ;
        } else {
        index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");
        
        var size = self.length;

        if (index < 0) {
          index += size;

          if (index < 0) {
            return nil;
          }
        }

        if (length === undefined) {
          if (index >= size || index < 0) {
            return nil;
          }

          return self[index];
        }
        else {
          length = $scope.get('Opal').$coerce_to(length, $scope.get('Integer'), "to_int");

          if (length < 0 || index > size || index < 0) {
            return nil;
          }

          return self.slice(index, index + length);
        }
      
      };
    };

    def['$[]='] = function(index, value, extra) {
      var $a, self = this, data = nil, length = nil;

      if ((($a = $scope.get('Range')['$==='](index)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = $scope.get('Array')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
          data = value.$to_a()
        } else if ((($a = value['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
          data = value.$to_ary().$to_a()
          } else {
          data = [value]
        };
        
        var size    = self.length,
            exclude = index.exclude,
            from    = $scope.get('Opal').$coerce_to(index.begin, $scope.get('Integer'), "to_int"),
            to      = $scope.get('Opal').$coerce_to(index.end, $scope.get('Integer'), "to_int");

        if (from < 0) {
          from += size;

          if (from < 0) {
            self.$raise($scope.get('RangeError'), "" + (index.$inspect()) + " out of range");
          }
        }

        if (to < 0) {
          to += size;
        }

        if (!exclude) {
          to += 1;
        }

        if (from > size) {
          for (var i = size; i < from; i++) {
            self[i] = nil;
          }
        }

        if (to < 0) {
          self.splice.apply(self, [from, 0].concat(data));
        }
        else {
          self.splice.apply(self, [from, to - from].concat(data));
        }

        return value;
      ;
        } else {
        if ((($a = extra === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
          length = 1
          } else {
          length = value;
          value = extra;
          if ((($a = $scope.get('Array')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
            data = value.$to_a()
          } else if ((($a = value['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
            data = value.$to_ary().$to_a()
            } else {
            data = [value]
          };
        };
        
        var size   = self.length,
            index  = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int"),
            length = $scope.get('Opal').$coerce_to(length, $scope.get('Integer'), "to_int"),
            old;

        if (index < 0) {
          old    = index;
          index += size;

          if (index < 0) {
            self.$raise($scope.get('IndexError'), "index " + (old) + " too small for array; minimum " + (-self.length));
          }
        }

        if (length < 0) {
          self.$raise($scope.get('IndexError'), "negative length (" + (length) + ")")
        }

        if (index > size) {
          for (var i = size; i < index; i++) {
            self[i] = nil;
          }
        }

        if (extra === undefined) {
          self[index] = value;
        }
        else {
          self.splice.apply(self, [index, length].concat(data));
        }

        return value;
      ;
      };
    };

    def.$assoc = function(object) {
      var self = this;

      
      for (var i = 0, length = self.length, item; i < length; i++) {
        if (item = self[i], item.length && (item[0])['$=='](object)) {
          return item;
        }
      }

      return nil;
    
    };

    def.$at = function(index) {
      var self = this;

      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");
      
      if (index < 0) {
        index += self.length;
      }

      if (index < 0 || index >= self.length) {
        return nil;
      }

      return self[index];
    
    };

    def.$bsearch = TMP_2 = function() {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("bsearch")
      };
      
      var min = 0,
          max = self.length,
          mid,
          val,
          ret,
          smaller = false,
          satisfied = nil;

      while (min < max) {
        mid = min + Math.floor((max - min) / 2);
        val = self[mid];
        ret = block(val);

        if (ret === $breaker) {
          return $breaker.$v;
        }
        else if (ret === true) {
          satisfied = val;
          smaller = true;
        }
        else if (ret === false || ret === nil) {
          smaller = false;
        }
        else if (ret.$$is_number) {
          if (ret === 0) { return val; }
          smaller = (ret < 0);
        }
        else {
          self.$raise($scope.get('TypeError'), "wrong argument type " + ((ret).$class()) + " (must be numeric, true, false or nil)")
        }

        if (smaller) { max = mid; } else { min = mid + 1; }
      }

      return satisfied;
    
    };

    def.$cycle = TMP_3 = function(n) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      if (n == null) {
        n = nil
      }
      TMP_3.$$p = null;
      if ((($a = ((($b = self['$empty?']()) !== false && $b !== nil) ? $b : n['$=='](0))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("cycle", n)
      };
      if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        while (true) {
          for (var i = 0, length = self.length; i < length; i++) {
            var value = Opal.yield1(block, self[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }
        }
      
        } else {
        n = $scope.get('Opal')['$coerce_to!'](n, $scope.get('Integer'), "to_int");
        
        if (n <= 0) {
          return self;
        }

        while (n > 0) {
          for (var i = 0, length = self.length; i < length; i++) {
            var value = Opal.yield1(block, self[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }

          n--;
        }
      
      };
      return self;
    };

    def.$clear = function() {
      var self = this;

      self.splice(0, self.length);
      return self;
    };

    def.$clone = function() {
      var self = this, copy = nil;

      copy = [];
      copy.$initialize_clone(self);
      return copy;
    };

    def.$dup = function() {
      var self = this, copy = nil;

      copy = [];
      copy.$initialize_dup(self);
      return copy;
    };

    def.$initialize_copy = function(other) {
      var self = this;

      return self.$replace(other);
    };

    def.$collect = TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect")
      };
      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, self[i]);

        if (value === $breaker) {
          return $breaker.$v;
        }

        result.push(value);
      }

      return result;
    
    };

    def['$collect!'] = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect!")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, self[i]);

        if (value === $breaker) {
          return $breaker.$v;
        }

        self[i] = value;
      }
    
      return self;
    };

    def.$combination = TMP_6 = function(n) {
      var $a, self = this, $iter = TMP_6.$$p, $yield = $iter || nil, num = nil;

      TMP_6.$$p = null;
      num = $scope.get('Opal')['$coerce_to!'](n, $scope.get('Integer'), "to_int");
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("combination", num)
      };
      
      var i, length, stack, chosen, lev, done, next;

      if (num === 0) {
        ((($a = Opal.yield1($yield, [])) === $breaker) ? $breaker.$v : $a)
      } else if (num === 1) {
        for (i = 0, length = self.length; i < length; i++) {
          ((($a = Opal.yield1($yield, [self[i]])) === $breaker) ? $breaker.$v : $a)
        }
      }
      else if (num === self.length) {
        ((($a = Opal.yield1($yield, self.slice())) === $breaker) ? $breaker.$v : $a)
      }
      else if (num >= 0 && num < self.length) {
        stack = [];
        for (i = 0; i <= num + 1; i++) {
          stack.push(0);
        }

        chosen = [];
        lev = 0;
        done = false;
        stack[0] = -1;

        while (!done) {
          chosen[lev] = self[stack[lev+1]];
          while (lev < num - 1) {
            lev++;
            next = stack[lev+1] = stack[lev] + 1;
            chosen[lev] = self[next];
          }
          ((($a = Opal.yield1($yield, chosen.slice())) === $breaker) ? $breaker.$v : $a)
          lev++;
          do {
            done = (lev === 0);
            stack[lev]++;
            lev--;
          } while ( stack[lev+1] + num === self.length + lev + 1 );
        }
      }
    ;
      return self;
    };

    def.$compact = function() {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length, item; i < length; i++) {
        if ((item = self[i]) !== nil) {
          result.push(item);
        }
      }

      return result;
    
    };

    def['$compact!'] = function() {
      var self = this;

      
      var original = self.length;

      for (var i = 0, length = self.length; i < length; i++) {
        if (self[i] === nil) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }

      return self.length === original ? nil : self;
    
    };

    def.$concat = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      for (var i = 0, length = other.length; i < length; i++) {
        self.push(other[i]);
      }
    
      return self;
    };

    def.$delete = TMP_7 = function(object) {
      var $a, self = this, $iter = TMP_7.$$p, $yield = $iter || nil;

      TMP_7.$$p = null;
      
      var original = self.length;

      for (var i = 0, length = original; i < length; i++) {
        if ((self[i])['$=='](object)) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }

      if (self.length === original) {
        if (($yield !== nil)) {
          return ((($a = Opal.yieldX($yield, [])) === $breaker) ? $breaker.$v : $a);
        }
        return nil;
      }
      return object;
    ;
    };

    def.$delete_at = function(index) {
      var self = this;

      
      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (index < 0) {
        index += self.length;
      }

      if (index < 0 || index >= self.length) {
        return nil;
      }

      var result = self[index];

      self.splice(index, 1);

      return result;
    ;
    };

    def.$delete_if = TMP_8 = function() {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      TMP_8.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("delete_if")
      };
      
      for (var i = 0, length = self.length, value; i < length; i++) {
        if ((value = block(self[i])) === $breaker) {
          return $breaker.$v;
        }

        if (value !== false && value !== nil) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }
    
      return self;
    };

    def.$drop = function(number) {
      var self = this;

      
      if (number < 0) {
        self.$raise($scope.get('ArgumentError'))
      }

      return self.slice(number);
    ;
    };

    Opal.defn(self, '$dup', def.$clone);

    def.$each = TMP_9 = function() {
      var self = this, $iter = TMP_9.$$p, block = $iter || nil;

      TMP_9.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, self[i]);

        if (value == $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def.$each_index = TMP_10 = function() {
      var self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_index")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, i);

        if (value === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def['$empty?'] = function() {
      var self = this;

      return self.length === 0;
    };

    def['$eql?'] = function(other) {
      var self = this;

      
      var recursed = {};

      function _eql(array, other) {
        var i, length, a, b;

        if (!other.$$is_array) {
          return false;
        }

        other = other.$to_a();

        if (array.length !== other.length) {
          return false;
        }

        recursed[(array).$object_id()] = true;

        for (i = 0, length = array.length; i < length; i++) {
          a = array[i];
          b = other[i];
          if (a.$$is_array) {
            if (b.$$is_array && b.length !== a.length) {
              return false;
            }
            if (!recursed.hasOwnProperty((a).$object_id())) {
              if (!_eql(a, b)) {
                return false;
              }
            }
          } else {
            if (!(a)['$eql?'](b)) {
              return false;
            }
          }
        }

        return true;
      }

      return _eql(self, other);
    
    };

    def.$fetch = TMP_11 = function(index, defaults) {
      var self = this, $iter = TMP_11.$$p, block = $iter || nil;

      TMP_11.$$p = null;
      
      var original = index;

      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (index < 0) {
        index += self.length;
      }

      if (index >= 0 && index < self.length) {
        return self[index];
      }

      if (block !== nil) {
        return block(original);
      }

      if (defaults != null) {
        return defaults;
      }

      if (self.length === 0) {
        self.$raise($scope.get('IndexError'), "index " + (original) + " outside of array bounds: 0...0")
      }
      else {
        self.$raise($scope.get('IndexError'), "index " + (original) + " outside of array bounds: -" + (self.length) + "..." + (self.length));
      }
    ;
    };

    def.$fill = TMP_12 = function(args) {
      var $a, self = this, $iter = TMP_12.$$p, block = $iter || nil, one = nil, two = nil, obj = nil, left = nil, right = nil;

      args = $slice.call(arguments, 0);
      TMP_12.$$p = null;
      if (block !== false && block !== nil) {
        if ((($a = args.length > 2) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (args.$length()) + " for 0..2)")};
        $a = Opal.to_ary(args), one = ($a[0] == null ? nil : $a[0]), two = ($a[1] == null ? nil : $a[1]);
        } else {
        if ((($a = args.length == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (0 for 1..3)")
        } else if ((($a = args.length > 3) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (args.$length()) + " for 1..3)")};
        $a = Opal.to_ary(args), obj = ($a[0] == null ? nil : $a[0]), one = ($a[1] == null ? nil : $a[1]), two = ($a[2] == null ? nil : $a[2]);
      };
      if ((($a = $scope.get('Range')['$==='](one)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if (two !== false && two !== nil) {
          self.$raise($scope.get('TypeError'), "length invalid with range")};
        left = $scope.get('Opal').$coerce_to(one.$begin(), $scope.get('Integer'), "to_int");
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          left += self.length;};
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('RangeError'), "" + (one.$inspect()) + " out of range")};
        right = $scope.get('Opal').$coerce_to(one.$end(), $scope.get('Integer'), "to_int");
        if ((($a = right < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          right += self.length;};
        if ((($a = one['$exclude_end?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          right += 1;
        };
        if ((($a = right <= left) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self};
      } else if (one !== false && one !== nil) {
        left = $scope.get('Opal').$coerce_to(one, $scope.get('Integer'), "to_int");
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          left += self.length;};
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          left = 0};
        if (two !== false && two !== nil) {
          right = $scope.get('Opal').$coerce_to(two, $scope.get('Integer'), "to_int");
          if ((($a = right == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self};
          right += left;
          } else {
          right = self.length
        };
        } else {
        left = 0;
        right = self.length;
      };
      if ((($a = left > self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        for (var i = self.length; i < right; i++) {
          self[i] = nil;
        }
      ;};
      if ((($a = right > self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.length = right};
      if (block !== false && block !== nil) {
        
        for (var length = self.length; left < right; left++) {
          var value = block(left);

          if (value === $breaker) {
            return $breaker.$v;
          }

          self[left] = value;
        }
      ;
        } else {
        
        for (var length = self.length; left < right; left++) {
          self[left] = obj;
        }
      ;
      };
      return self;
    };

    def.$first = function(count) {
      var self = this;

      
      if (count == null) {
        return self.length === 0 ? nil : self[0];
      }

      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");

      if (count < 0) {
        self.$raise($scope.get('ArgumentError'), "negative array size");
      }

      return self.slice(0, count);
    
    };

    def.$flatten = function(level) {
      var self = this;

      
      var object_id = (self).$object_id();

      function _flatten(array, level) {
        var array = (array).$to_a(),
            result = [],
            i, length,
            item, ary;

        for (i = 0, length = array.length; i < length; i++) {
          item = array[i];

          if (!$scope.get('Opal')['$respond_to?'](item, "to_ary")) {
            result.push(item);
            continue;
          }

          ary = (item).$to_ary();

          if (ary === nil) {
            result.push(item);
            continue;
          }

          if (!ary.$$is_array) {
            self.$raise($scope.get('TypeError'));
          }

          if (object_id === (ary).$object_id()) {
            self.$raise($scope.get('ArgumentError'));
          }

          switch (level) {
          case undefined:
            result.push.apply(result, _flatten(ary));
            break;
          case 0:
            result.push(ary);
            break;
          default:
            result.push.apply(result, _flatten(ary, level - 1));
          }
        }
        return result;
      }

      if (level !== undefined) {
        level = $scope.get('Opal').$coerce_to(level, $scope.get('Integer'), "to_int");
      }

      return _flatten(self, level);
    ;
    };

    def['$flatten!'] = function(level) {
      var self = this;

      
      var flattened = self.$flatten(level);

      if (self.length == flattened.length) {
        for (var i = 0, length = self.length; i < length; i++) {
          if (self[i] !== flattened[i]) {
            break;
          }
        }

        if (i == length) {
          return nil;
        }
      }

      self.$replace(flattened);
    ;
      return self;
    };

    def.$hash = function() {
      var self = this;

      
      var hash = ['A'],
          item;
      for (var i = 0, length = self.length; i < length; i++) {
        item = self[i];
        if (item.$$is_array && (self)['$eql?'](item)) {
          hash.push('self');
        } else {
          hash.push(item.$hash());
        }
      }
      return hash.join(',');
    
    };

    def['$include?'] = function(member) {
      var self = this;

      
      for (var i = 0, length = self.length; i < length; i++) {
        if ((self[i])['$=='](member)) {
          return true;
        }
      }

      return false;
    
    };

    def.$index = TMP_13 = function(object) {
      var self = this, $iter = TMP_13.$$p, block = $iter || nil;

      TMP_13.$$p = null;
      
      if (object != null) {
        for (var i = 0, length = self.length; i < length; i++) {
          if ((self[i])['$=='](object)) {
            return i;
          }
        }
      }
      else if (block !== nil) {
        for (var i = 0, length = self.length, value; i < length; i++) {
          if ((value = block(self[i])) === $breaker) {
            return $breaker.$v;
          }

          if (value !== false && value !== nil) {
            return i;
          }
        }
      }
      else {
        return self.$enum_for("index");
      }

      return nil;
    
    };

    def.$insert = function(index, objects) {
      var self = this;

      objects = $slice.call(arguments, 1);
      
      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (objects.length > 0) {
        if (index < 0) {
          index += self.length + 1;

          if (index < 0) {
            self.$raise($scope.get('IndexError'), "" + (index) + " is out of bounds");
          }
        }
        if (index > self.length) {
          for (var i = self.length; i < index; i++) {
            self.push(nil);
          }
        }

        self.splice.apply(self, [index, 0].concat(objects));
      }
    ;
      return self;
    };

    def.$inspect = function() {
      var self = this;

      
      var result = [],
          id     = self.$__id__();

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self['$[]'](i);

        if ((item).$__id__() === id) {
          result.push('[...]');
        }
        else {
          result.push((item).$inspect());
        }
      }

      return '[' + result.join(', ') + ']';
    ;
    };

    def.$join = function(sep) {
      var $a, self = this;
      if ($gvars[","] == null) $gvars[","] = nil;

      if (sep == null) {
        sep = nil
      }
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ""};
      if ((($a = sep === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        sep = $gvars[","]};
      
      var result = [];
      var object_id = (self).$object_id();

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if ($scope.get('Opal')['$respond_to?'](item, "to_str")) {
          var tmp = (item).$to_str();

          if (tmp !== nil) {
            result.push((tmp).$to_s());

            continue;
          }
        }

        if ($scope.get('Opal')['$respond_to?'](item, "to_ary")) {
          var tmp = (item).$to_ary();

          if (object_id === (tmp).$object_id()) {
            self.$raise($scope.get('ArgumentError'));
          }

          if (tmp !== nil) {
            result.push((tmp).$join(sep));

            continue;
          }
        }

        if ($scope.get('Opal')['$respond_to?'](item, "to_s")) {
          var tmp = (item).$to_s();

          if (tmp !== nil) {
            result.push(tmp);

            continue;
          }
        }

        self.$raise($scope.get('NoMethodError'), "" + ($scope.get('Opal').$inspect(item)) + " doesn't respond to #to_str, #to_ary or #to_s");
      }

      if (sep === nil) {
        return result.join('');
      }
      else {
        return result.join($scope.get('Opal')['$coerce_to!'](sep, $scope.get('String'), "to_str").$to_s());
      }
    ;
    };

    def.$keep_if = TMP_14 = function() {
      var self = this, $iter = TMP_14.$$p, block = $iter || nil;

      TMP_14.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("keep_if")
      };
      
      for (var i = 0, length = self.length, value; i < length; i++) {
        if ((value = block(self[i])) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }
    
      return self;
    };

    def.$last = function(count) {
      var self = this;

      
      if (count == null) {
        return self.length === 0 ? nil : self[self.length - 1];
      }

      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");

      if (count < 0) {
        self.$raise($scope.get('ArgumentError'), "negative array size");
      }

      if (count > self.length) {
        count = self.length;
      }

      return self.slice(self.length - count, self.length);
    
    };

    def.$length = function() {
      var self = this;

      return self.length;
    };

    Opal.defn(self, '$map', def.$collect);

    Opal.defn(self, '$map!', def['$collect!']);

    def.$pop = function(count) {
      var $a, self = this;

      if ((($a = count === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil};
        return self.pop();};
      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");
      if ((($a = count < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative array size")};
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if ((($a = count > self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.splice(0, self.length);
        } else {
        return self.splice(self.length - count, self.length);
      };
    };

    def.$product = TMP_15 = function(args) {
      var $a, self = this, $iter = TMP_15.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_15.$$p = null;
      
      var result = (block !== nil) ? null : [],
          n = args.length + 1,
          counters = new Array(n),
          lengths  = new Array(n),
          arrays   = new Array(n),
          i, m, subarray, len, resultlen = 1;

      arrays[0] = self;
      for (i = 1; i < n; i++) {
        arrays[i] = $scope.get('Opal').$coerce_to(args[i - 1], $scope.get('Array'), "to_ary");
      }

      for (i = 0; i < n; i++) {
        len = arrays[i].length;
        if (len === 0) {
          return result || self;
        }
        resultlen *= len;
        if (resultlen > 2147483647) {
          self.$raise($scope.get('RangeError'), "too big to product")
        }
        lengths[i] = len;
        counters[i] = 0;
      }

      outer_loop: for (;;) {
        subarray = [];
        for (i = 0; i < n; i++) {
          subarray.push(arrays[i][counters[i]]);
        }
        if (result) {
          result.push(subarray);
        } else {
          ((($a = Opal.yield1(block, subarray)) === $breaker) ? $breaker.$v : $a)
        }
        m = n - 1;
        counters[m]++;
        while (counters[m] === lengths[m]) {
          counters[m] = 0;
          if (--m < 0) break outer_loop;
          counters[m]++;
        }
      }

      return result || self;
    ;
    };

    def.$push = function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      
      for (var i = 0, length = objects.length; i < length; i++) {
        self.push(objects[i]);
      }
    
      return self;
    };

    def.$rassoc = function(object) {
      var self = this;

      
      for (var i = 0, length = self.length, item; i < length; i++) {
        item = self[i];

        if (item.length && item[1] !== undefined) {
          if ((item[1])['$=='](object)) {
            return item;
          }
        }
      }

      return nil;
    
    };

    def.$reject = TMP_16 = function() {
      var self = this, $iter = TMP_16.$$p, block = $iter || nil;

      TMP_16.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reject")
      };
      
      var result = [];

      for (var i = 0, length = self.length, value; i < length; i++) {
        if ((value = block(self[i])) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          result.push(self[i]);
        }
      }
      return result;
    
    };

    def['$reject!'] = TMP_17 = function() {
      var $a, $b, self = this, $iter = TMP_17.$$p, block = $iter || nil, original = nil;

      TMP_17.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reject!")
      };
      original = self.$length();
      ($a = ($b = self).$delete_if, $a.$$p = block.$to_proc(), $a).call($b);
      if (self.$length()['$=='](original)) {
        return nil
        } else {
        return self
      };
    };

    def.$replace = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      self.splice(0, self.length);
      self.push.apply(self, other);
    
      return self;
    };

    def.$reverse = function() {
      var self = this;

      return self.slice(0).reverse();
    };

    def['$reverse!'] = function() {
      var self = this;

      return self.reverse();
    };

    def.$reverse_each = TMP_18 = function() {
      var $a, $b, self = this, $iter = TMP_18.$$p, block = $iter || nil;

      TMP_18.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reverse_each")
      };
      ($a = ($b = self.$reverse()).$each, $a.$$p = block.$to_proc(), $a).call($b);
      return self;
    };

    def.$rindex = TMP_19 = function(object) {
      var self = this, $iter = TMP_19.$$p, block = $iter || nil;

      TMP_19.$$p = null;
      
      if (object != null) {
        for (var i = self.length - 1; i >= 0; i--) {
          if ((self[i])['$=='](object)) {
            return i;
          }
        }
      }
      else if (block !== nil) {
        for (var i = self.length - 1, value; i >= 0; i--) {
          if ((value = block(self[i])) === $breaker) {
            return $breaker.$v;
          }

          if (value !== false && value !== nil) {
            return i;
          }
        }
      }
      else if (object == null) {
        return self.$enum_for("rindex");
      }

      return nil;
    
    };

    def.$rotate = function(n) {
      var self = this;

      if (n == null) {
        n = 1
      }
      n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
      
      var ary, idx, firstPart, lastPart;
      
      if (self.length === 1) {
        return self.slice();
      }
      if (self.length === 0) {
        return [];
      }
      
      ary = self.slice();
      idx = n % ary.length;
      
      firstPart = ary.slice(idx);
      lastPart = ary.slice(0, idx);
      return firstPart.concat(lastPart);
    
    };

    def['$rotate!'] = function(cnt) {
      var $a, self = this, ary = nil;

      if (cnt == null) {
        cnt = 1
      }
      if ((($a = self['$frozen?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('RuntimeError'), "can't modify frozen Array")};
      
      if (self.length === 0 || self.length === 1) {
        return self;
      }
    
      cnt = $scope.get('Opal').$coerce_to(cnt, $scope.get('Integer'), "to_int");
      ary = self.$rotate(cnt);
      return self.$replace(ary);
    };

    def.$sample = function(n) {
      var $a, $b, TMP_20, self = this;

      if (n == null) {
        n = nil
      }
      if ((($a = ($b = n['$!'](), $b !== false && $b !== nil ?self['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      if ((($a = (($b = n !== false && n !== nil) ? self['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if (n !== false && n !== nil) {
        return ($a = ($b = ($range(1, n, false))).$map, $a.$$p = (TMP_20 = function(){var self = TMP_20.$$s || this;

        return self['$[]'](self.$rand(self.$length()))}, TMP_20.$$s = self, TMP_20), $a).call($b)
        } else {
        return self['$[]'](self.$rand(self.$length()))
      };
    };

    def.$select = TMP_21 = function() {
      var self = this, $iter = TMP_21.$$p, block = $iter || nil;

      TMP_21.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("select")
      };
      
      var result = [];

      for (var i = 0, length = self.length, item, value; i < length; i++) {
        item = self[i];

        if ((value = Opal.yield1(block, item)) === $breaker) {
          return $breaker.$v;
        }

        if (value !== false && value !== nil) {
          result.push(item);
        }
      }

      return result;
    
    };

    def['$select!'] = TMP_22 = function() {
      var $a, $b, self = this, $iter = TMP_22.$$p, block = $iter || nil;

      TMP_22.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("select!")
      };
      
      var original = self.length;
      ($a = ($b = self).$keep_if, $a.$$p = block.$to_proc(), $a).call($b);
      return self.length === original ? nil : self;
    
    };

    def.$shift = function(count) {
      var $a, self = this;

      if ((($a = count === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil};
        return self.shift();};
      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");
      if ((($a = count < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative array size")};
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      return self.splice(0, count);
    };

    Opal.defn(self, '$size', def.$length);

    def.$shuffle = function() {
      var self = this;

      return self.$clone()['$shuffle!']();
    };

    def['$shuffle!'] = function() {
      var self = this;

      
      for (var i = self.length - 1; i > 0; i--) {
        var tmp = self[i],
            j   = Math.floor(Math.random() * (i + 1));

        self[i] = self[j];
        self[j] = tmp;
      }
    
      return self;
    };

    Opal.defn(self, '$slice', def['$[]']);

    def['$slice!'] = function(index, length) {
      var self = this;

      
      if (index < 0) {
        index += self.length;
      }

      if (length != null) {
        return self.splice(index, length);
      }

      if (index < 0 || index >= self.length) {
        return nil;
      }

      return self.splice(index, 1)[0];
    
    };

    def.$sort = TMP_23 = function() {
      var $a, self = this, $iter = TMP_23.$$p, block = $iter || nil;

      TMP_23.$$p = null;
      if ((($a = self.length > 1) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return self
      };
      
      if (!(block !== nil)) {
        block = function(a, b) {
          return (a)['$<=>'](b);
        };
      }

      try {
        return self.slice().sort(function(x, y) {
          var ret = block(x, y);

          if (ret === $breaker) {
            throw $breaker;
          }
          else if (ret === nil) {
            self.$raise($scope.get('ArgumentError'), "comparison of " + ((x).$inspect()) + " with " + ((y).$inspect()) + " failed");
          }

          return $rb_gt(ret, 0) ? 1 : ($rb_lt(ret, 0) ? -1 : 0);
        });
      }
      catch (e) {
        if (e === $breaker) {
          return $breaker.$v;
        }
        else {
          throw e;
        }
      }
    ;
    };

    def['$sort!'] = TMP_24 = function() {
      var $a, $b, self = this, $iter = TMP_24.$$p, block = $iter || nil;

      TMP_24.$$p = null;
      
      var result;

      if ((block !== nil)) {
        result = ($a = ($b = (self.slice())).$sort, $a.$$p = block.$to_proc(), $a).call($b);
      }
      else {
        result = (self.slice()).$sort();
      }

      self.length = 0;
      for(var i = 0, length = result.length; i < length; i++) {
        self.push(result[i]);
      }

      return self;
    ;
    };

    def.$take = function(count) {
      var self = this;

      
      if (count < 0) {
        self.$raise($scope.get('ArgumentError'));
      }

      return self.slice(0, count);
    ;
    };

    def.$take_while = TMP_25 = function() {
      var self = this, $iter = TMP_25.$$p, block = $iter || nil;

      TMP_25.$$p = null;
      
      var result = [];

      for (var i = 0, length = self.length, item, value; i < length; i++) {
        item = self[i];

        if ((value = block(item)) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          return result;
        }

        result.push(item);
      }

      return result;
    
    };

    def.$to_a = function() {
      var self = this;

      return self;
    };

    Opal.defn(self, '$to_ary', def.$to_a);

    def.$to_h = function() {
      var self = this;

      
      var i, len = self.length, ary, key, val, hash = $hash2([], {});

      for (i = 0; i < len; i++) {
        ary = $scope.get('Opal')['$coerce_to?'](self[i], $scope.get('Array'), "to_ary");
        if (!ary.$$is_array) {
          self.$raise($scope.get('TypeError'), "wrong element type " + ((ary).$class()) + " at " + (i) + " (expected array)")
        }
        if (ary.length !== 2) {
          self.$raise($scope.get('ArgumentError'), "wrong array length at " + (i) + " (expected 2, was " + ((ary).$length()) + ")")
        }
        key = ary[0];
        val = ary[1];
        hash.$store(key, val);
      }

      return hash;
    ;
    };

    Opal.defn(self, '$to_s', def.$inspect);

    def.$transpose = function() {
      var $a, $b, TMP_26, self = this, result = nil, max = nil;

      if ((($a = self['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      result = [];
      max = nil;
      ($a = ($b = self).$each, $a.$$p = (TMP_26 = function(row){var self = TMP_26.$$s || this, $a, $b, TMP_27;
if (row == null) row = nil;
      if ((($a = $scope.get('Array')['$==='](row)) !== nil && (!$a.$$is_boolean || $a == true))) {
          row = row.$to_a()
          } else {
          row = $scope.get('Opal').$coerce_to(row, $scope.get('Array'), "to_ary").$to_a()
        };
        ((($a = max) !== false && $a !== nil) ? $a : max = row.length);
        if ((($a = (row.length)['$=='](max)['$!']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('IndexError'), "element size differs (" + (row.length) + " should be " + (max))};
        return ($a = ($b = (row.length)).$times, $a.$$p = (TMP_27 = function(i){var self = TMP_27.$$s || this, $a, $b, $c, entry = nil;
if (i == null) i = nil;
        entry = (($a = i, $b = result, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, []))));
          return entry['$<<'](row.$at(i));}, TMP_27.$$s = self, TMP_27), $a).call($b);}, TMP_26.$$s = self, TMP_26), $a).call($b);
      return result;
    };

    def.$uniq = function() {
      var self = this;

      
      var result = [],
          seen   = {};

      for (var i = 0, length = self.length, item, hash; i < length; i++) {
        item = self[i];
        hash = item;

        if (!seen[hash]) {
          seen[hash] = true;

          result.push(item);
        }
      }

      return result;
    
    };

    def['$uniq!'] = function() {
      var self = this;

      
      var original = self.length,
          seen     = {};

      for (var i = 0, length = original, item, hash; i < length; i++) {
        item = self[i];
        hash = item;

        if (!seen[hash]) {
          seen[hash] = true;
        }
        else {
          self.splice(i, 1);

          length--;
          i--;
        }
      }

      return self.length === original ? nil : self;
    
    };

    def.$unshift = function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      
      for (var i = objects.length - 1; i >= 0; i--) {
        self.unshift(objects[i]);
      }
    
      return self;
    };

    def.$values_at = function(args) {
      var $a, $b, TMP_28, self = this, out = nil;

      args = $slice.call(arguments, 0);
      out = [];
      ($a = ($b = args).$each, $a.$$p = (TMP_28 = function(elem){var self = TMP_28.$$s || this, $a, $b, TMP_29, finish = nil, start = nil, i = nil;
if (elem == null) elem = nil;
      if ((($a = elem['$kind_of?']($scope.get('Range'))) !== nil && (!$a.$$is_boolean || $a == true))) {
          finish = $scope.get('Opal').$coerce_to(elem.$last(), $scope.get('Integer'), "to_int");
          start = $scope.get('Opal').$coerce_to(elem.$first(), $scope.get('Integer'), "to_int");
          
          if (start < 0) {
            start = start + self.length;
            return nil;;
          }
        
          
          if (finish < 0) {
            finish = finish + self.length;
          }
          if (elem['$exclude_end?']()) {
            finish--;
          }
          if (finish < start) {
            return nil;;
          }
        
          return ($a = ($b = start).$upto, $a.$$p = (TMP_29 = function(i){var self = TMP_29.$$s || this;
if (i == null) i = nil;
          return out['$<<'](self.$at(i))}, TMP_29.$$s = self, TMP_29), $a).call($b, finish);
          } else {
          i = $scope.get('Opal').$coerce_to(elem, $scope.get('Integer'), "to_int");
          return out['$<<'](self.$at(i));
        }}, TMP_28.$$s = self, TMP_28), $a).call($b);
      return out;
    };

    return (def.$zip = TMP_30 = function(others) {
      var $a, self = this, $iter = TMP_30.$$p, block = $iter || nil;

      others = $slice.call(arguments, 0);
      TMP_30.$$p = null;
      
      var result = [], size = self.length, part, o, i, j, jj;

      for (j = 0, jj = others.length; j < jj; j++) {
        o = others[j];
        if (!o.$$is_array) {
          others[j] = (((($a = $scope.get('Opal')['$coerce_to?'](o, $scope.get('Array'), "to_ary")) !== false && $a !== nil) ? $a : $scope.get('Opal')['$coerce_to!'](o, $scope.get('Enumerator'), "each"))).$to_a();
        }
      }

      for (i = 0; i < size; i++) {
        part = [self[i]];

        for (j = 0, jj = others.length; j < jj; j++) {
          o = others[j][i];

          if (o == null) {
            o = nil;
          }

          part[j + 1] = o;
        }

        result[i] = part;
      }

      if (block !== nil) {
        for (i = 0; i < size; i++) {
          block(result[i]);
        }

        return nil;
      }

      return result;
    
    }, nil) && 'zip';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/array/inheritance"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_times(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs * rhs : lhs['$*'](rhs);
  }
  function $rb_minus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs - rhs : lhs['$-'](rhs);
  }
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$allocate', '$initialize', '$to_proc', '$__send__', '$clone', '$respond_to?', '$==', '$eql?', '$inspect', '$hash', '$class', '$slice', '$uniq', '$flatten']);
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defs(self, '$inherited', function(klass) {
      var self = this, replace = nil;

      replace = $scope.get('Class').$new((($scope.get('Array')).$$scope.get('Wrapper')));
      
      klass.$$proto         = replace.$$proto;
      klass.$$proto.$$class = klass;
      klass.$$alloc         = replace.$$alloc;
      klass.$$parent        = (($scope.get('Array')).$$scope.get('Wrapper'));

      klass.$allocate = replace.$allocate;
      klass.$new      = replace.$new;
      klass["$[]"]    = replace["$[]"];
    
    }), nil) && 'inherited'
  })(self, null);
  return (function($base, $super) {
    function $Wrapper(){};
    var self = $Wrapper = $klass($base, $super, 'Wrapper', $Wrapper);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5;

    def.literal = nil;
    def.$$is_array = true;

    Opal.defs(self, '$allocate', TMP_1 = function(array) {
      var self = this, $iter = TMP_1.$$p, $yield = $iter || nil, obj = nil;

      if (array == null) {
        array = []
      }
      TMP_1.$$p = null;
      obj = Opal.find_super_dispatcher(self, 'allocate', TMP_1, null, $Wrapper).apply(self, []);
      obj.literal = array;
      return obj;
    });

    Opal.defs(self, '$new', TMP_2 = function(args) {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil, obj = nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      obj = self.$allocate();
      ($a = ($b = obj).$initialize, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      return obj;
    });

    Opal.defs(self, '$[]', function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      return self.$allocate(objects);
    });

    def.$initialize = TMP_3 = function(args) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      return self.literal = ($a = ($b = $scope.get('Array')).$new, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
    };

    def.$method_missing = TMP_4 = function(args) {
      var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil, result = nil;

      args = $slice.call(arguments, 0);
      TMP_4.$$p = null;
      result = ($a = ($b = self.literal).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      if ((($a = result === self.literal) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self
        } else {
        return result
      };
    };

    def.$initialize_copy = function(other) {
      var self = this;

      return self.literal = (other.literal).$clone();
    };

    def['$respond_to?'] = TMP_5 = function(name) {var $zuper = $slice.call(arguments, 0);
      var $a, self = this, $iter = TMP_5.$$p, $yield = $iter || nil;

      TMP_5.$$p = null;
      return ((($a = Opal.find_super_dispatcher(self, 'respond_to?', TMP_5, $iter).apply(self, $zuper)) !== false && $a !== nil) ? $a : self.literal['$respond_to?'](name));
    };

    def['$=='] = function(other) {
      var self = this;

      return self.literal['$=='](other);
    };

    def['$eql?'] = function(other) {
      var self = this;

      return self.literal['$eql?'](other);
    };

    def.$to_a = function() {
      var self = this;

      return self.literal;
    };

    def.$to_ary = function() {
      var self = this;

      return self;
    };

    def.$inspect = function() {
      var self = this;

      return self.literal.$inspect();
    };

    def.$hash = function() {
      var self = this;

      return self.literal.$hash();
    };

    def['$*'] = function(other) {
      var self = this;

      
      var result = $rb_times(self.literal, other);

      if (result.$$is_array) {
        return self.$class().$allocate(result)
      }
      else {
        return result;
      }
    ;
    };

    def['$[]'] = function(index, length) {
      var self = this;

      
      var result = self.literal.$slice(index, length);

      if (result.$$is_array && (index.$$is_range || length !== undefined)) {
        return self.$class().$allocate(result)
      }
      else {
        return result;
      }
    ;
    };

    Opal.defn(self, '$slice', def['$[]']);

    def.$uniq = function() {
      var self = this;

      return self.$class().$allocate(self.literal.$uniq());
    };

    def.$flatten = function(level) {
      var self = this;

      return self.$class().$allocate(self.literal.$flatten(level));
    };

    def['$-'] = function(other) {
      var self = this;

      return $rb_minus(self.literal, other);
    };

    return (def['$+'] = function(other) {
      var self = this;

      return $rb_plus(self.literal, other);
    }, nil) && '+';
  })($scope.get('Array'), null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/hash"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$coerce_to?', '$[]', '$merge!', '$allocate', '$raise', '$!', '$==', '$call', '$coerce_to!', '$lambda?', '$abs', '$arity', '$enum_for', '$inspect', '$flatten', '$eql?', '$===', '$clone', '$to_proc', '$alias_method']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_8, TMP_9, TMP_10, TMP_11, TMP_12, TMP_13;

    def.proc = def.none = nil;
    self.$include($scope.get('Enumerable'));

    def.$$is_hash = true;

    Opal.defs(self, '$[]', function(argv) {
      var self = this;

      argv = $slice.call(arguments, 0);
      
      var hash, i, argc = argv.length;

      if (argc === 1) {
        hash = $scope.get('Opal')['$coerce_to?'](argv['$[]'](0), $scope.get('Hash'), "to_hash");
        if (hash !== nil) {
          return self.$allocate()['$merge!'](hash);
        }

        argv = $scope.get('Opal')['$coerce_to?'](argv['$[]'](0), $scope.get('Array'), "to_ary");
        if (argv === nil) {
          self.$raise($scope.get('ArgumentError'), "odd number of arguments for Hash")
        }

        argc = argv.length;
        hash = self.$allocate();

        for (i = 0; i < argc; i++) {
          if (!argv[i].$$is_array) continue;
          switch(argv[i].length) {
          case 1:
            hash.$store(argv[i][0], nil);
            break;
          case 2:
            hash.$store(argv[i][0], argv[i][1]);
            break;
          default:
            self.$raise($scope.get('ArgumentError'), "invalid number of elements (" + (argv[i].length) + " for 1..2)")
          }
        }

        return hash;
      }

      if (argc % 2 !== 0) {
        self.$raise($scope.get('ArgumentError'), "odd number of arguments for Hash")
      }

      hash = self.$allocate();

      for (i = 0; i < argc; i += 2) {
        hash.$store(argv[i], argv[i + 1]);
      }

      return hash;
    ;
    });

    Opal.defs(self, '$allocate', function() {
      var self = this;

      
      var hash = new self.$$alloc;

      hash.map  = {};
      hash.smap = {};
      hash.keys = [];
      hash.none = nil;
      hash.proc = nil;

      return hash;
    
    });

    def.$initialize = TMP_1 = function(defaults) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      
      self.none = (defaults === undefined ? nil : defaults);
      self.proc = block;
    
      return self;
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (self === other) {
        return true;
      }

      if (!other.keys || !other.smap || !other.map) {
        return false;
      }

      if (self.keys.length !== other.keys.length) {
        return false;
      }

      var _map  = self.map,
          smap  = self.smap,
          _map2 = other.map,
          smap2 = other.smap,
          map, map2, key, khash, value, value2;

      for (var i = 0, length = self.keys.length; i < length; i++) {
        key = self.keys[i];

        if (key.$$is_string) {
          khash = key;
          map   = smap;
          map2  = smap2;
        } else {
          khash = key.$hash();
          map   = _map;
          map2  = _map2;
        }

        value  = map[khash];
        if (value === undefined) console.log('==', key, self);
        value2 = map2[khash];

        if (value2 === undefined || ((value)['$=='](value2))['$!']()) {
          return false;
        }
      }

      return true;
    
    };

    def['$[]'] = function(key) {
      var self = this;

      
      var map, khash;

      if (key.$$is_string) {
        map = self.smap;
        khash = key;
      } else {
        map = self.map;
        khash = key.$hash();
      }

      if (map === undefined) { console.log(self, '[] --> key:', key, khash, map) }


      if (Opal.hasOwnProperty.call(map, khash)) {
        return map[khash];
      }

      var proc = self.proc;

      if (proc !== nil) {
        return (proc).$call(self, key);
      }

      return self.none;
    
    };

    def['$[]='] = function(key, value) {
      var self = this;

      
      var map, khash, value;

      if (key.$$is_string) {
        map = self.smap;
        khash = key;
      } else {
        map = self.map;
        khash = key.$hash();
      }

      if (!Opal.hasOwnProperty.call(map, khash)) {
        self.keys.push(key);
      }

      map[khash] = value;

      return value;
    
    };

    def.$assoc = function(object) {
      var self = this;

      
      var keys = self.keys,
          map, key, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if ((key)['$=='](object)) {
          if (key.$$is_string) {
            map = self.smap;
            khash = key;
          } else {
            map = self.map;
            khash = key.$hash();
          }

          return [key, map[khash]];
        }
      }

      return nil;
    
    };

    def.$clear = function() {
      var self = this;

      
      self.map = {};
      self.smap = {};
      self.keys = [];
      return self;
    
    };

    def.$clone = function() {
      var self = this;

      
      var _map  = {},
          smap  = {},
          _map2 = self.map,
          smap2 = self.smap,
          keys  = [],
          map, map2, key, khash, value;

      for (var i = 0, length = self.keys.length; i < length; i++) {
        key   = self.keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        value = map2[khash];

        keys.push(key);
        map[khash] = value;
      }

      var clone = new self.$$class.$$alloc();

      clone.map  = _map;
      clone.smap = smap;
      clone.keys = keys;
      clone.none = self.none;
      clone.proc = self.proc;

      return clone;
    
    };

    def.$default = function(val) {
      var self = this;

      
      if (val !== undefined && self.proc !== nil) {
        return self.proc.$call(self, val);
      }
      return self.none;
    ;
    };

    def['$default='] = function(object) {
      var self = this;

      
      self.proc = nil;
      return (self.none = object);
    
    };

    def.$default_proc = function() {
      var self = this;

      return self.proc;
    };

    def['$default_proc='] = function(proc) {
      var self = this;

      
      if (proc !== nil) {
        proc = $scope.get('Opal')['$coerce_to!'](proc, $scope.get('Proc'), "to_proc");

        if (proc['$lambda?']() && proc.$arity().$abs() != 2) {
          self.$raise($scope.get('TypeError'), "default_proc takes two arguments");
        }
      }
      self.none = nil;
      return (self.proc = proc);
    ;
    };

    def.$delete = TMP_2 = function(key) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      
      var result, map, khash;

      if (key.$$is_string) {
        map = self.smap;
        khash = key;
      } else {
        map = self.map;
        khash = key.$hash();
      }

      result = map[khash];

      if (result != null) {
        delete map[khash];
        self.keys.$delete(key);

        return result;
      }

      if (block !== nil) {
        return block.$call(key);
      }
      return nil;
    
    };

    def.$delete_if = TMP_3 = function() {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("delete_if")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, key, value, obj, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }
        obj = map[khash];
        value = block(key, obj);

        if (value === $breaker) {
          return $breaker.$v;
        }

        if (value !== false && value !== nil) {
          keys.splice(i, 1);
          delete map[khash];

          length--;
          i--;
        }
      }

      return self;
    
    };

    Opal.defn(self, '$dup', def.$clone);

    def.$each = TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        value = Opal.yield1(block, [key, map[khash]]);

        if (value === $breaker) {
          return $breaker.$v;
        }
      }

      return self;
    
    };

    def.$each_key = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each_key")
      };
      
      var keys = self.keys, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (block(key) === $breaker) {
          return $breaker.$v;
        }
      }

      return self;
    
    };

    Opal.defn(self, '$each_pair', def.$each);

    def.$each_value = TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each_value")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys, key, map, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        if (block(map[khash]) === $breaker) {
          return $breaker.$v;
        }
      }

      return self;
    
    };

    def['$empty?'] = function() {
      var self = this;

      return self.keys.length === 0;
    };

    Opal.defn(self, '$eql?', def['$==']);

    def.$fetch = TMP_7 = function(key, defaults) {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      var map, khash, value;

      if (key.$$is_string) {
        khash = key;
        map = self.smap;
      } else {
        khash = key.$hash();
        map = self.map;
      }

      value = map[khash];

      if (value != null) {
        return value;
      }

      if (block !== nil) {
        var value;

        if ((value = block(key)) === $breaker) {
          return $breaker.$v;
        }

        return value;
      }

      if (defaults != null) {
        return defaults;
      }

      self.$raise($scope.get('KeyError'), "key not found: " + (key.$inspect()));
    
    };

    def.$flatten = function(level) {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          result = [],
          map, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];

        result.push(key);

        if (value.$$is_array) {
          if (level == null || level === 1) {
            result.push(value);
          }
          else {
            result = result.concat((value).$flatten(level - 1));
          }
        }
        else {
          result.push(value);
        }
      }

      return result;
    
    };

    def['$has_key?'] = function(key) {
      var self = this;

      
      var keys = self.keys,
          map, khash;

      if (key.$$is_string) {
        khash = key;
        map = self.smap;
      } else {
        khash = key.$hash();
        map = self.map;
      }

      if (Opal.hasOwnProperty.call(map, khash)) {
        for (var i = 0, length = keys.length; i < length; i++) {
          if (!(key['$eql?'](keys[i]))['$!']()) {
            return true;
          }
        }
      }

      return false;
    
    };

    def['$has_value?'] = function(value) {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys, key, map, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        if ((map[khash])['$=='](value)) {
          return true;
        }
      }

      return false;
    
    };

    var hash_ids = null;

    def.$hash = function() {
      var self = this;

      
      var top = (hash_ids === null);
      try {
        var key, value,
            hash = ['Hash'],
            keys = self.keys,
            id = self.$object_id(),
            counter = 0;

        if (top) {
          hash_ids = {}
        }

        if (hash_ids.hasOwnProperty(id)) {
          return 'self';
        }

        hash_ids[id] = true;

        for (var i = 0, length = keys.length; i < length; i++) {
          key   = keys[i];
          value = key.$$is_string ? self.smap[key] : self.map[key.$hash()];
          key   = key.$hash();
          value = (typeof(value) === 'undefined') ? '' : value.$hash();
          hash.push([key,value]);
        }

        return hash.sort().join();
      } finally {
        if (top) {
          hash_ids = null;
        }
      }
    
    };

    Opal.defn(self, '$include?', def['$has_key?']);

    def.$index = function(object) {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, khash, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        if ((map[khash])['$=='](object)) {
          return key;
        }
      }

      return nil;
    
    };

    def.$indexes = function(keys) {
      var self = this;

      keys = $slice.call(arguments, 0);
      
      var result = [],
          _map = self.map,
          smap = self.smap,
          map, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];

        if (value != null) {
          result.push(value);
        }
        else {
          result.push(self.none);
        }
      }

      return result;
    
    };

    Opal.defn(self, '$indices', def.$indexes);

    var inspect_ids = null;

    def.$inspect = function() {
      var self = this;

      
      var top = (inspect_ids === null);
      try {

        var key, value,
            inspect = [],
            keys = self.keys,
            id = self.$object_id(),
            counter = 0;

        if (top) {
          inspect_ids = {}
        }

        if (inspect_ids.hasOwnProperty(id)) {
          return '{...}';
        }

        inspect_ids[id] = true;

        for (var i = 0, length = keys.length; i < length; i++) {
          key   = keys[i];
          value = key.$$is_string ? self.smap[key] : self.map[key.$hash()];
          key   = key.$inspect();
          value = value.$inspect();
          inspect.push(key + '=>' + value);
        }

        return '{' + inspect.join(', ') + '}';
      } finally {

        if (top) {
          inspect_ids = null;
        }
      }
    
    };

    def.$invert = function() {
      var self = this;

      
      var result = Opal.hash(),
          keys = self.keys,
          _map = self.map,
          smap = self.smap,
          keys2 = result.keys,
          _map2 = result.map,
          smap2 = result.smap,
          map, map2, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        value = map[khash];
        keys2.push(value);

        if (value.$$is_string) {
          map2 = smap2;
          khash = value;
        } else {
          map2 = _map2;
          khash = value.$hash();
        }

        map2[khash] = key;
      }

      return result;
    
    };

    def.$keep_if = TMP_8 = function() {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      TMP_8.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("keep_if")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, key, khash, value, keep;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];
        keep  = block(key, value);

        if (keep === $breaker) {
          return $breaker.$v;
        }

        if (keep === false || keep === nil) {
          keys.splice(i, 1);
          delete map[khash];

          length--;
          i--;
        }
      }

      return self;
    
    };

    Opal.defn(self, '$key', def.$index);

    Opal.defn(self, '$key?', def['$has_key?']);

    def.$keys = function() {
      var self = this;

      return self.keys.slice(0);
    };

    def.$length = function() {
      var self = this;

      return self.keys.length;
    };

    Opal.defn(self, '$member?', def['$has_key?']);

    def.$merge = TMP_9 = function(other) {
      var $a, $b, self = this, $iter = TMP_9.$$p, block = $iter || nil, cloned = nil;

      TMP_9.$$p = null;
      if ((($a = $scope.get('Hash')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        other = $scope.get('Opal')['$coerce_to!'](other, $scope.get('Hash'), "to_hash")
      };
      cloned = self.$clone();
      ($a = ($b = cloned)['$merge!'], $a.$$p = block.$to_proc(), $a).call($b, other);
      return cloned;
    };

    def['$merge!'] = TMP_10 = function(other) {
      var self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      
      if (! $scope.get('Hash')['$==='](other)) {
        other = $scope.get('Opal')['$coerce_to!'](other, $scope.get('Hash'), "to_hash");
      }

      var keys  = self.keys,
          _map  = self.map,
          smap  = self.smap,
          keys2 = other.keys,
          _map2 = other.map,
          smap2 = other.smap,
          map, map2, key, khash, value, value2;

      if (block === nil) {
        for (var i = 0, length = keys2.length; i < length; i++) {
          key = keys2[i];

          if (key.$$is_string) {
            khash = key;
            map = smap;
            map2 = smap2;
          } else {
            khash = key.$hash();
            map = _map;
            map2 = _map2;
          }

          if (map[khash] == null) {
            keys.push(key);
          }

          map[khash] = map2[khash];
        }
      }
      else {
        for (var i = 0, length = keys2.length; i < length; i++) {
          key    = keys2[i];

          if (key.$$is_string) {
            khash = key;
            map = smap;
            map2 = smap2;
          } else {
            khash = key.$hash();
            map = _map;
            map2 = _map2;
          }

          value  = map[khash];
          value2 = map2[khash];

          if (value == null) {
            keys.push(key);
            map[khash] = value2;
          }
          else {
            map[khash] = block(key, value, value2);
          }
        }
      }

      return self;
    ;
    };

    def.$rassoc = function(object) {
      var self = this;

      
      var keys = self.keys,
          _map = self.map,
          smap = self.smap,
          key, khash, value, map;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i]

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];

        if ((value)['$=='](object)) {
          return [key, value];
        }
      }

      return nil;
    
    };

    def.$reject = TMP_11 = function() {
      var self = this, $iter = TMP_11.$$p, block = $iter || nil;

      TMP_11.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("reject")
      };
      
      var keys   = self.keys,
          _map    = self.map,
          smap    = self.smap,
          result = Opal.hash(),
          _map2   = result.map,
          smap2   = result.smap,
          keys2  = result.keys,
          map, map2, key, khash, object, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        object = map[khash];

        if ((value = block(key, object)) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          keys2.push(key);
          map2[khash] = object;
        }
      }

      return result;
    
    };

    def.$replace = function(other) {
      var self = this;

      
      var keys  = self.keys = [],
          _map  = self.map  = {},
          smap  = self.smap = {},
          _map2 = other.map,
          smap2 = other.smap,
          key, khash, map, map2;

      for (var i = 0, length = other.keys.length; i < length; i++) {
        key = other.keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        keys.push(key);
        map[khash] = map2[khash];
      }

      return self;
    
    };

    def.$select = TMP_12 = function() {
      var self = this, $iter = TMP_12.$$p, block = $iter || nil;

      TMP_12.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("select")
      };
      
      var keys   = self.keys,
          _map   = self.map,
          smap   = self.smap,
          result = Opal.hash(),
          _map2  = result.map,
          smap2  = result.smap,
          keys2  = result.keys,
          map, map2, key, khash, value, object;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        value = map[khash];
        object = block(key, value);

        if (object === $breaker) {
          return $breaker.$v;
        }

        if (object !== false && object !== nil) {
          keys2.push(key);
          map2[khash] = value;
        }
      }

      return result;
    
    };

    def['$select!'] = TMP_13 = function() {
      var self = this, $iter = TMP_13.$$p, block = $iter || nil;

      TMP_13.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("select!")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          result = nil,
          key, khash, value, object, map;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];
        object = block(key, value);

        if (object === $breaker) {
          return $breaker.$v;
        }

        if (object === false || object === nil) {
          keys.splice(i, 1);
          delete map[khash];

          length--;
          i--;
          result = self
        }
      }

      return result;
    
    };

    def.$shift = function() {
      var self = this;

      
      var keys = self.keys,
          _map = self.map,
          smap = self.smap,
          map, key, khash, value;

      if (keys.length) {
        key = keys[0];
        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }
        value = map[khash];

        delete map[khash];
        keys.splice(0, 1);

        return [key, value];
      }

      return nil;
    
    };

    Opal.defn(self, '$size', def.$length);

    self.$alias_method("store", "[]=");

    def.$to_a = function() {
      var self = this;

      
      var keys = self.keys,
          _map = self.map,
          smap = self.smap,
          result = [],
          map, key, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        result.push([key, map[khash]]);
      }

      return result;
    
    };

    def.$to_h = function() {
      var self = this;

      
      if (self.$$class === Opal.Hash) {
        return self
      }

      var hash   = new Opal.Hash.$$alloc,
          cloned = self.$clone();

      hash.map  = cloned.map;
      hash.smap = cloned.smap;
      hash.keys = cloned.keys;
      hash.none = cloned.none;
      hash.proc = cloned.proc;

      return hash;
    ;
    };

    def.$to_hash = function() {
      var self = this;

      return self;
    };

    Opal.defn(self, '$to_s', def.$inspect);

    Opal.defn(self, '$update', def['$merge!']);

    Opal.defn(self, '$value?', def['$has_value?']);

    Opal.defn(self, '$values_at', def.$indexes);

    return (def.$values = function() {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          result = [],
          map, khash, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        result.push(map[khash]);
      }

      return result;
    
    }, nil) && 'values';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/string"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_divide(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs / rhs : lhs['$/'](rhs);
  }
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$include', '$coerce_to?', '$coerce_to', '$raise', '$===', '$format', '$to_s', '$respond_to?', '$to_str', '$<=>', '$==', '$=~', '$new', '$empty?', '$ljust', '$ceil', '$rjust', '$floor', '$to_a', '$each_char', '$to_proc', '$coerce_to!', '$initialize_clone', '$initialize_dup', '$enum_for', '$chomp', '$[]', '$to_i', '$class', '$each_line', '$match', '$captures', '$proc', '$shift', '$__send__', '$succ', '$escape']);
  self.$require("corelib/comparable");
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_8, TMP_10;

    def.length = nil;
    self.$include($scope.get('Comparable'));

    def.$$is_string = true;

    def.$__id__ = function() {
      var self = this;

      return self.toString();
    };

    Opal.defn(self, '$object_id', def.$__id__);

    Opal.defs(self, '$try_convert', function(what) {
      var self = this;

      return $scope.get('Opal')['$coerce_to?'](what, $scope.get('String'), "to_str");
    });

    Opal.defs(self, '$new', function(str) {
      var self = this;

      if (str == null) {
        str = ""
      }
      str = $scope.get('Opal').$coerce_to(str, $scope.get('String'), "to_str");
      return new String(str);
    });

    def.$initialize = function(str) {
      var self = this;

      
      if (str === undefined) {
        return self;
      }
    
      return self.$raise($scope.get('NotImplementedError'), "Mutable strings are not supported in Opal.");
    };

    def['$%'] = function(data) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](data)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = self).$format.apply($a, [self].concat(data))
        } else {
        return self.$format(self, data)
      };
    };

    def['$*'] = function(count) {
      var self = this;

      
      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");

      if (count < 0) {
        self.$raise($scope.get('ArgumentError'), "negative argument")
      }

      if (count === 0) {
        return '';
      }

      var result = '',
          string = self.toString();

      // All credit for the bit-twiddling magic code below goes to Mozilla
      // polyfill implementation of String.prototype.repeat() posted here:
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/repeat

      if (string.length * count >= 1 << 28) {
        self.$raise($scope.get('RangeError'), "multiply count must not overflow maximum string size")
      }

      for (;;) {
        if ((count & 1) === 1) {
          result += string;
        }
        count >>>= 1;
        if (count === 0) {
          break;
        }
        string += string;
      }

      return result;
    ;
    };

    def['$+'] = function(other) {
      var self = this;

      other = $scope.get('Opal').$coerce_to(other, $scope.get('String'), "to_str");
      return self + other.$to_s();
    };

    def['$<=>'] = function(other) {
      var $a, self = this;

      if ((($a = other['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_str().$to_s();
        return self > other ? 1 : (self < other ? -1 : 0);
        } else {
        
        var cmp = other['$<=>'](self);

        if (cmp === nil) {
          return nil;
        }
        else {
          return cmp > 0 ? -1 : (cmp < 0 ? 1 : 0);
        }
      ;
      };
    };

    def['$<<'] = function(other) {
      var self = this;

      return self.$raise($scope.get('NotImplementedError'), "#<< not supported. Mutable String methods are not supported in Opal.");
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (other.$$is_string) {
        return self.toString() === other.toString();
      }
      if ($scope.get('Opal')['$respond_to?'](other, "to_str")) {
        return other['$=='](self);
      }
      return false;
    ;
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$===', def['$==']);

    def['$=~'] = function(other) {
      var self = this;

      
      if (other.$$is_string) {
        self.$raise($scope.get('TypeError'), "type mismatch: String given");
      }

      return other['$=~'](self);
    ;
    };

    def['$[]'] = function(index, length) {
      var self = this;

      
      var size = self.length;

      if (index.$$is_range) {
        var exclude = index.exclude,
            length  = $scope.get('Opal').$coerce_to(index.end, $scope.get('Integer'), "to_int"),
            index   = $scope.get('Opal').$coerce_to(index.begin, $scope.get('Integer'), "to_int");

        if (Math.abs(index) > size) {
          return nil;
        }

        if (index < 0) {
          index += size;
        }

        if (length < 0) {
          length += size;
        }

        if (!exclude) {
          length += 1;
        }

        length = length - index;

        if (length < 0) {
          length = 0;
        }

        return self.substr(index, length);
      }


      if (index.$$is_string) {
        if (length != null) {
          self.$raise($scope.get('TypeError'))
        }
        return self.indexOf(index) !== -1 ? index : nil;
      }


      if (index.$$is_regexp) {
        var match = self.match(index);

        if (match === null) {
          $gvars["~"] = nil
          return nil;
        }

        $gvars["~"] = $scope.get('MatchData').$new(index, match)

        if (length == null) {
          return match[0];
        }

        length = $scope.get('Opal').$coerce_to(length, $scope.get('Integer'), "to_int");

        if (length < 0 && -length < match.length) {
          return match[length += match.length];
        }

        if (length >= 0 && length < match.length) {
          return match[length];
        }

        return nil;
      }


      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (index < 0) {
        index += size;
      }

      if (length == null) {
        if (index >= size || index < 0) {
          return nil;
        }
        return self.substr(index, 1);
      }

      length = $scope.get('Opal').$coerce_to(length, $scope.get('Integer'), "to_int");

      if (length < 0) {
        return nil;
      }

      if (index > size || index < 0) {
        return nil;
      }

      return self.substr(index, length);
    ;
    };

    def.$capitalize = function() {
      var self = this;

      return self.charAt(0).toUpperCase() + self.substr(1).toLowerCase();
    };

    Opal.defn(self, '$capitalize!', def['$<<']);

    def.$casecmp = function(other) {
      var self = this;

      other = $scope.get('Opal').$coerce_to(other, $scope.get('String'), "to_str").$to_s();
      
      var ascii_only = /^[\x00-\x7F]*$/;
      if (ascii_only.test(self) && ascii_only.test(other)) {
        self = self.toLowerCase();
        other = other.toLowerCase();
      }
    
      return self['$<=>'](other);
    };

    def.$center = function(width, padstr) {
      var $a, self = this;

      if (padstr == null) {
        padstr = " "
      }
      width = $scope.get('Opal').$coerce_to(width, $scope.get('Integer'), "to_int");
      padstr = $scope.get('Opal').$coerce_to(padstr, $scope.get('String'), "to_str").$to_s();
      if ((($a = padstr['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "zero width padding")};
      if ((($a = width <= self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      
      var ljustified = self.$ljust($rb_divide(($rb_plus(width, self.length)), 2).$ceil(), padstr),
          rjustified = self.$rjust($rb_divide(($rb_plus(width, self.length)), 2).$floor(), padstr);

      return rjustified + ljustified.slice(self.length);
    ;
    };

    def.$chars = TMP_1 = function() {
      var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$each_char().$to_a()
      };
      return ($a = ($b = self).$each_char, $a.$$p = block.$to_proc(), $a).call($b);
    };

    def.$chomp = function(separator) {
      var $a, self = this;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      if ((($a = separator === nil || self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      separator = $scope.get('Opal')['$coerce_to!'](separator, $scope.get('String'), "to_str").$to_s();
      
      if (separator === "\n") {
        return self.replace(/\r?\n?$/, '');
      }
      else if (separator === "") {
        return self.replace(/(\r?\n)+$/, '');
      }
      else if (self.length > separator.length) {
        var tail = self.substr(self.length - separator.length, separator.length);

        if (tail === separator) {
          return self.substr(0, self.length - separator.length);
        }
      }
    
      return self;
    };

    Opal.defn(self, '$chomp!', def['$<<']);

    def.$chop = function() {
      var self = this;

      
      var length = self.length;

      if (length <= 1) {
        return "";
      }

      if (self.charAt(length - 1) === "\n" && self.charAt(length - 2) === "\r") {
        return self.substr(0, length - 2);
      }
      else {
        return self.substr(0, length - 1);
      }
    
    };

    Opal.defn(self, '$chop!', def['$<<']);

    def.$chr = function() {
      var self = this;

      return self.charAt(0);
    };

    def.$clone = function() {
      var self = this, copy = nil;

      copy = self.slice();
      copy.$initialize_clone(self);
      return copy;
    };

    def.$dup = function() {
      var self = this, copy = nil;

      copy = self.slice();
      copy.$initialize_dup(self);
      return copy;
    };

    def.$count = function(sets) {
      var self = this;

      sets = $slice.call(arguments, 0);
      
      if (sets.length === 0) {
        self.$raise($scope.get('ArgumentError'), "ArgumentError: wrong number of arguments (0 for 1+)")
      }
      var char_class = char_class_from_char_sets(sets);
      if (char_class === null) {
        return 0;
      }
      return self.length - self.replace(new RegExp(char_class, 'g'), '').length;
    ;
    };

    def.$delete = function(sets) {
      var self = this;

      sets = $slice.call(arguments, 0);
      
      if (sets.length === 0) {
        self.$raise($scope.get('ArgumentError'), "ArgumentError: wrong number of arguments (0 for 1+)")
      }
      var char_class = char_class_from_char_sets(sets);
      if (char_class === null) {
        return self;
      }
      return self.replace(new RegExp(char_class, 'g'), '');
    ;
    };

    Opal.defn(self, '$dup', def.$clone);

    def.$downcase = function() {
      var self = this;

      return self.toLowerCase();
    };

    Opal.defn(self, '$downcase!', def['$<<']);

    def.$each_char = TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_char")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        ((($a = Opal.yield1(block, self.charAt(i))) === $breaker) ? $breaker.$v : $a);
      }
    
      return self;
    };

    def.$each_line = TMP_3 = function(separator) {
      var $a, self = this, $iter = TMP_3.$$p, $yield = $iter || nil;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      TMP_3.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each_line", separator)
      };
      
      if (separator === nil) {
        ((($a = Opal.yield1($yield, self)) === $breaker) ? $breaker.$v : $a);
        return self;
      }

      separator = $scope.get('Opal').$coerce_to(separator, $scope.get('String'), "to_str")

      if (separator.length === 0) {
        for (var a = self.split(/(\n{2,})/), i = 0, n = a.length; i < n; i += 2) {
          if (a[i] || a[i + 1]) {
            ((($a = Opal.yield1($yield, (a[i] || "") + (a[i + 1] || ""))) === $breaker) ? $breaker.$v : $a);
          }
        }
        return self;
      }

      var chomped  = self.$chomp(separator),
          trailing = self.length != chomped.length,
          splitted = chomped.split(separator);

      for (var i = 0, length = splitted.length; i < length; i++) {
        if (i < length - 1 || trailing) {
          ((($a = Opal.yield1($yield, splitted[i] + separator)) === $breaker) ? $breaker.$v : $a);
        }
        else {
          ((($a = Opal.yield1($yield, splitted[i])) === $breaker) ? $breaker.$v : $a);
        }
      }
    ;
      return self;
    };

    def['$empty?'] = function() {
      var self = this;

      return self.length === 0;
    };

    def['$end_with?'] = function(suffixes) {
      var self = this;

      suffixes = $slice.call(arguments, 0);
      
      for (var i = 0, length = suffixes.length; i < length; i++) {
        var suffix = $scope.get('Opal').$coerce_to(suffixes[i], $scope.get('String'), "to_str").$to_s();

        if (self.length >= suffix.length &&
            self.substr(self.length - suffix.length, suffix.length) == suffix) {
          return true;
        }
      }
    
      return false;
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$equal?', def['$===']);

    def.$gsub = TMP_4 = function(pattern, replacement) {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      
      var result = '', match_data = nil, index = 0, match, _replacement;

      if (pattern.$$is_regexp) {
        pattern = new RegExp(pattern.source, 'gm' + (pattern.ignoreCase ? 'i' : ''));
      } else {
        pattern = $scope.get('Opal').$coerce_to(pattern, $scope.get('String'), "to_str");
        pattern = new RegExp(pattern.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gm');
      }

      while (true) {
        match = pattern.exec(self);

        if (match === null) {
          $gvars["~"] = nil
          result += self.slice(index);
          break;
        }

        match_data = $scope.get('MatchData').$new(pattern, match);

        if (replacement === undefined) {
          if (block === nil) {
            self.$raise($scope.get('ArgumentError'), "wrong number of arguments (1 for 2)")
          }
          _replacement = block(match[0]);
        }
        else if (replacement.$$is_hash) {
          _replacement = (replacement)['$[]'](match[0]).$to_s();
        }
        else {
          if (!replacement.$$is_string) {
            replacement = $scope.get('Opal').$coerce_to(replacement, $scope.get('String'), "to_str");
          }
          _replacement = replacement.replace(/([\\]+)([0-9+&`'])/g, function (original, slashes, command) {
            if (slashes.length % 2 === 0) {
              return original;
            }
            switch (command) {
            case "+":
              for (var i = match.length - 1; i > 0; i--) {
                if (match[i] !== undefined) {
                  return slashes.slice(1) + match[i];
                }
              }
              return '';
            case "&": return slashes.slice(1) + match[0];
            case "`": return slashes.slice(1) + self.slice(0, match.index);
            case "'": return slashes.slice(1) + self.slice(match.index + match[0].length);
            default:  return slashes.slice(1) + (match[command] || '');
            }
          }).replace(/\\\\/g, '\\');
        }

        if (pattern.lastIndex === match.index) {
          result += (_replacement + self.slice(index, match.index + 1))
          pattern.lastIndex += 1;
        }
        else {
          result += (self.slice(index, match.index) + _replacement)
        }
        index = pattern.lastIndex;
      }

      $gvars["~"] = match_data
      return result;
    
    };

    Opal.defn(self, '$gsub!', def['$<<']);

    def.$hash = function() {
      var self = this;

      return self.toString();
    };

    def.$hex = function() {
      var self = this;

      return self.$to_i(16);
    };

    def['$include?'] = function(other) {
      var $a, self = this;

      
      if (other.$$is_string) {
        return self.indexOf(other) !== -1;
      }
    
      if ((($a = other['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "no implicit conversion of " + (other.$class()) + " into String")
      };
      return self.indexOf(other.$to_str()) !== -1;
    };

    def.$index = function(search, offset) {
      var self = this;

      
      var index,
          match,
          regex;

      if (offset === undefined) {
        offset = 0;
      } else {
        offset = $scope.get('Opal').$coerce_to(offset, $scope.get('Integer'), "to_int");
        if (offset < 0) {
          offset += self.length;
          if (offset < 0) {
            return nil;
          }
        }
      }

      if (search.$$is_regexp) {
        regex = new RegExp(search.source, 'gm' + (search.ignoreCase ? 'i' : ''));
        while (true) {
          match = regex.exec(self);
          if (match === null) {
            $gvars["~"] = nil;
            index = -1;
            break;
          }
          if (match.index >= offset) {
            $gvars["~"] = $scope.get('MatchData').$new(regex, match)
            index = match.index;
            break;
          }
          regex.lastIndex = match.index + 1;
        }
      } else {
        search = $scope.get('Opal').$coerce_to(search, $scope.get('String'), "to_str");
        if (search.length === 0 && offset > self.length) {
          index = -1;
        } else {
          index = self.indexOf(search, offset);
        }
      }

      return index === -1 ? nil : index;
    
    };

    def.$inspect = function() {
      var self = this;

      
      var escapable = /[\\\"\x00-\x1f\x7f-\x9f\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
          meta = {
            '\u0007': '\\a',
            '\u001b': '\\e',
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '\v': '\\v',
            '"' : '\\"',
            '\\': '\\\\'
          },
          escaped = self.replace(escapable, function (chr) {
            return meta[chr] || '\\u' + ('0000' + chr.charCodeAt(0).toString(16).toUpperCase()).slice(-4);
          });
      return '"' + escaped.replace(/\#[\$\@\{]/g, '\\$&') + '"';
    
    };

    def.$intern = function() {
      var self = this;

      return self;
    };

    def.$lines = TMP_5 = function(separator) {
      var $a, $b, self = this, $iter = TMP_5.$$p, block = $iter || nil, e = nil;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      TMP_5.$$p = null;
      e = ($a = ($b = self).$each_line, $a.$$p = block.$to_proc(), $a).call($b, separator);
      if (block !== false && block !== nil) {
        return self
        } else {
        return e.$to_a()
      };
    };

    def.$length = function() {
      var self = this;

      return self.length;
    };

    def.$ljust = function(width, padstr) {
      var $a, self = this;

      if (padstr == null) {
        padstr = " "
      }
      width = $scope.get('Opal').$coerce_to(width, $scope.get('Integer'), "to_int");
      padstr = $scope.get('Opal').$coerce_to(padstr, $scope.get('String'), "to_str").$to_s();
      if ((($a = padstr['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "zero width padding")};
      if ((($a = width <= self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      
      var index  = -1,
          result = "";

      width -= self.length;

      while (++index < width) {
        result += padstr;
      }

      return self + result.slice(0, width);
    
    };

    def.$lstrip = function() {
      var self = this;

      return self.replace(/^\s*/, '');
    };

    Opal.defn(self, '$lstrip!', def['$<<']);

    def.$match = TMP_6 = function(pattern, pos) {
      var $a, $b, self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      if ((($a = ((($b = $scope.get('String')['$==='](pattern)) !== false && $b !== nil) ? $b : pattern['$respond_to?']("to_str"))) !== nil && (!$a.$$is_boolean || $a == true))) {
        pattern = $scope.get('Regexp').$new(pattern.$to_str())};
      if ((($a = $scope.get('Regexp')['$==='](pattern)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "wrong argument type " + (pattern.$class()) + " (expected Regexp)")
      };
      return ($a = ($b = pattern).$match, $a.$$p = block.$to_proc(), $a).call($b, self, pos);
    };

    def.$next = function() {
      var self = this;

      
      var i = self.length;
      if (i === 0) {
        return '';
      }
      var result = self;
      var first_alphanum_char_index = self.search(/[a-zA-Z0-9]/);
      var carry = false;
      var code;
      while (i--) {
        code = self.charCodeAt(i);
        if ((code >= 48 && code <= 57) ||
          (code >= 65 && code <= 90) ||
          (code >= 97 && code <= 122)) {
          switch (code) {
          case 57:
            carry = true;
            code = 48;
            break;
          case 90:
            carry = true;
            code = 65;
            break;
          case 122:
            carry = true;
            code = 97;
            break;
          default:
            carry = false;
            code += 1;
          }
        } else {
          if (first_alphanum_char_index === -1) {
            if (code === 255) {
              carry = true;
              code = 0;
            } else {
              carry = false;
              code += 1;
            }
          } else {
            carry = true;
          }
        }
        result = result.slice(0, i) + String.fromCharCode(code) + result.slice(i + 1);
        if (carry && (i === 0 || i === first_alphanum_char_index)) {
          switch (code) {
          case 65:
            break;
          case 97:
            break;
          default:
            code += 1;
          }
          if (i === 0) {
            result = String.fromCharCode(code) + result;
          } else {
            result = result.slice(0, i) + String.fromCharCode(code) + result.slice(i);
          }
          carry = false;
        }
        if (!carry) {
          break;
        }
      }
      return result;
    
    };

    Opal.defn(self, '$next!', def['$<<']);

    def.$oct = function() {
      var self = this;

      
      var result,
          string = self,
          radix = 8;

      if (/^\s*_/.test(string)) {
        return 0;
      }

      string = string.replace(/^(\s*[+-]?)(0[bodx]?)(.+)$/i, function (original, head, flag, tail) {
        switch (tail.charAt(0)) {
        case '+':
        case '-':
          return original;
        case '0':
          if (tail.charAt(1) === 'x' && flag === '0x') {
            return original;
          }
        }
        switch (flag) {
        case '0b':
          radix = 2;
          break;
        case '0':
        case '0o':
          radix = 8;
          break;
        case '0d':
          radix = 10;
          break;
        case '0x':
          radix = 16;
          break;
        }
        return head + tail;
      });

      result = parseInt(string.replace(/_(?!_)/g, ''), radix);
      return isNaN(result) ? 0 : result;
    
    };

    def.$ord = function() {
      var self = this;

      return self.charCodeAt(0);
    };

    def.$partition = function(sep) {
      var self = this;

      
      var i, m;

      if (sep.$$is_regexp) {
        m = sep.exec(self);
        if (m === null) {
          i = -1;
        } else {
          $scope.get('MatchData').$new(sep, m);
          sep = m[0];
          i = m.index;
        }
      } else {
        sep = $scope.get('Opal').$coerce_to(sep, $scope.get('String'), "to_str");
        i = self.indexOf(sep);
      }

      if (i === -1) {
        return [self, '', ''];
      }

      return [
        self.slice(0, i),
        self.slice(i, i + sep.length),
        self.slice(i + sep.length)
      ];
    
    };

    def.$reverse = function() {
      var self = this;

      return self.split('').reverse().join('');
    };

    Opal.defn(self, '$reverse!', def['$<<']);

    def.$rindex = function(search, offset) {
      var self = this;

      
      var i, m, r, _m;

      if (offset === undefined) {
        offset = self.length;
      } else {
        offset = $scope.get('Opal').$coerce_to(offset, $scope.get('Integer'), "to_int");
        if (offset < 0) {
          offset += self.length;
          if (offset < 0) {
            return nil;
          }
        }
      }

      if (search.$$is_regexp) {
        m = null;
        r = new RegExp(search.source, 'gm' + (search.ignoreCase ? 'i' : ''));
        while (true) {
          _m = r.exec(self);
          if (_m === null || _m.index > offset) {
            break;
          }
          m = _m;
          r.lastIndex = m.index + 1;
        }
        if (m === null) {
          $gvars["~"] = nil
          i = -1;
        } else {
          $scope.get('MatchData').$new(r, m);
          i = m.index;
        }
      } else {
        search = $scope.get('Opal').$coerce_to(search, $scope.get('String'), "to_str");
        i = self.lastIndexOf(search, offset);
      }

      return i === -1 ? nil : i;
    
    };

    def.$rjust = function(width, padstr) {
      var $a, self = this;

      if (padstr == null) {
        padstr = " "
      }
      width = $scope.get('Opal').$coerce_to(width, $scope.get('Integer'), "to_int");
      padstr = $scope.get('Opal').$coerce_to(padstr, $scope.get('String'), "to_str").$to_s();
      if ((($a = padstr['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "zero width padding")};
      if ((($a = width <= self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      
      var chars     = Math.floor(width - self.length),
          patterns  = Math.floor(chars / padstr.length),
          result    = Array(patterns + 1).join(padstr),
          remaining = chars - result.length;

      return result + padstr.slice(0, remaining) + self;
    
    };

    def.$rpartition = function(sep) {
      var self = this;

      
      var i, m, r, _m;

      if (sep.$$is_regexp) {
        m = null;
        r = new RegExp(sep.source, 'gm' + (sep.ignoreCase ? 'i' : ''));

        while (true) {
          _m = r.exec(self);
          if (_m === null) {
            break;
          }
          m = _m;
          r.lastIndex = m.index + 1;
        }

        if (m === null) {
          i = -1;
        } else {
          $scope.get('MatchData').$new(r, m);
          sep = m[0];
          i = m.index;
        }

      } else {
        sep = $scope.get('Opal').$coerce_to(sep, $scope.get('String'), "to_str");
        i = self.lastIndexOf(sep);
      }

      if (i === -1) {
        return ['', '', self];
      }

      return [
        self.slice(0, i),
        self.slice(i, i + sep.length),
        self.slice(i + sep.length)
      ];
    
    };

    def.$rstrip = function() {
      var self = this;

      return self.replace(/[\s\u0000]*$/, '');
    };

    def.$scan = TMP_7 = function(pattern) {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      var result = [],
          match_data = nil,
          match;

      if (pattern.$$is_regexp) {
        pattern = new RegExp(pattern.source, 'gm' + (pattern.ignoreCase ? 'i' : ''));
      } else {
        pattern = $scope.get('Opal').$coerce_to(pattern, $scope.get('String'), "to_str");
        pattern = new RegExp(pattern.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gm');
      }

      while ((match = pattern.exec(self)) != null) {
        match_data = $scope.get('MatchData').$new(pattern, match);
        if (block === nil) {
          match.length == 1 ? result.push(match[0]) : result.push((match_data).$captures());
        } else {
          match.length == 1 ? block(match[0]) : block.call(self, (match_data).$captures());
        }
        if (pattern.lastIndex === match.index) {
          pattern.lastIndex += 1;
        }
      }

      $gvars["~"] = match_data

      return (block !== nil ? self : result);
    
    };

    Opal.defn(self, '$size', def.$length);

    Opal.defn(self, '$slice', def['$[]']);

    Opal.defn(self, '$slice!', def['$<<']);

    def.$split = function(pattern, limit) {
      var $a, self = this;
      if ($gvars[";"] == null) $gvars[";"] = nil;

      
      if (self.length === 0) {
        return [];
      }

      if (limit === undefined) {
        limit = 0;
      } else {
        limit = $scope.get('Opal')['$coerce_to!'](limit, $scope.get('Integer'), "to_int");
        if (limit === 1) {
          return [self];
        }
      }

      if (pattern === undefined || pattern === nil) {
        pattern = ((($a = $gvars[";"]) !== false && $a !== nil) ? $a : " ");
      }

      var result = [],
          string = self.toString(),
          index = 0,
          match,
          i;

      if (pattern.$$is_regexp) {
        pattern = new RegExp(pattern.source, 'gm' + (pattern.ignoreCase ? 'i' : ''));
      } else {
        pattern = $scope.get('Opal').$coerce_to(pattern, $scope.get('String'), "to_str").$to_s();
        if (pattern === ' ') {
          pattern = /\s+/gm;
          string = string.replace(/^\s+/, '');
        } else {
          pattern = new RegExp(pattern.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gm');
        }
      }

      result = string.split(pattern);

      if (result.length === 1 && result[0] === string) {
        return result;
      }

      while ((i = result.indexOf(undefined)) !== -1) {
        result.splice(i, 1);
      }

      if (limit === 0) {
        while (result[result.length - 1] === '') {
          result.length -= 1;
        }
        return result;
      }

      match = pattern.exec(string);

      if (limit < 0) {
        if (match !== null && match[0] === '' && pattern.source.indexOf('(?=') === -1) {
          for (i = 0; i < match.length; i++) {
            result.push('');
          }
        }
        return result;
      }

      if (match !== null && match[0] === '') {
        result.splice(limit - 1, result.length - 1, result.slice(limit - 1).join(''));
        return result;
      }

      i = 0;
      while (match !== null) {
        i++;
        index = pattern.lastIndex;
        if (i + 1 === limit) {
          break;
        }
        match = pattern.exec(string);
      }

      result.splice(limit - 1, result.length - 1, string.slice(index));
      return result;
    
    };

    def.$squeeze = function(sets) {
      var self = this;

      sets = $slice.call(arguments, 0);
      
      if (sets.length === 0) {
        return self.replace(/(.)\1+/g, '$1');
      }
      var char_class = char_class_from_char_sets(sets);
      if (char_class === null) {
        return self;
      }
      return self.replace(new RegExp('(' + char_class + ')\\1+', 'g'), '$1');
    
    };

    Opal.defn(self, '$squeeze!', def['$<<']);

    def['$start_with?'] = function(prefixes) {
      var self = this;

      prefixes = $slice.call(arguments, 0);
      
      for (var i = 0, length = prefixes.length; i < length; i++) {
        var prefix = $scope.get('Opal').$coerce_to(prefixes[i], $scope.get('String'), "to_str").$to_s();

        if (self.indexOf(prefix) === 0) {
          return true;
        }
      }

      return false;
    
    };

    def.$strip = function() {
      var self = this;

      return self.replace(/^\s*/, '').replace(/[\s\u0000]*$/, '');
    };

    Opal.defn(self, '$strip!', def['$<<']);

    def.$sub = TMP_8 = function(pattern, replacement) {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      TMP_8.$$p = null;
      
      if (!pattern.$$is_regexp) {
        pattern = $scope.get('Opal').$coerce_to(pattern, $scope.get('String'), "to_str");
        pattern = new RegExp(pattern.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'));
      }

      var result = pattern.exec(self);

      if (result === null) {
        $gvars["~"] = nil
        return self.toString();
      }

      $scope.get('MatchData').$new(pattern, result)

      if (replacement === undefined) {
        if (block === nil) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (1 for 2)")
        }
        return self.slice(0, result.index) + block(result[0]) + self.slice(result.index + result[0].length);
      }

      if (replacement.$$is_hash) {
        return self.slice(0, result.index) + (replacement)['$[]'](result[0]).$to_s() + self.slice(result.index + result[0].length);
      }

      replacement = $scope.get('Opal').$coerce_to(replacement, $scope.get('String'), "to_str");

      replacement = replacement.replace(/([\\]+)([0-9+&`'])/g, function (original, slashes, command) {
        if (slashes.length % 2 === 0) {
          return original;
        }
        switch (command) {
        case "+":
          for (var i = result.length - 1; i > 0; i--) {
            if (result[i] !== undefined) {
              return slashes.slice(1) + result[i];
            }
          }
          return '';
        case "&": return slashes.slice(1) + result[0];
        case "`": return slashes.slice(1) + self.slice(0, result.index);
        case "'": return slashes.slice(1) + self.slice(result.index + result[0].length);
        default:  return slashes.slice(1) + (result[command] || '');
        }
      }).replace(/\\\\/g, '\\');

      return self.slice(0, result.index) + replacement + self.slice(result.index + result[0].length);
    ;
    };

    Opal.defn(self, '$sub!', def['$<<']);

    Opal.defn(self, '$succ', def.$next);

    Opal.defn(self, '$succ!', def['$<<']);

    def.$sum = function(n) {
      var self = this;

      if (n == null) {
        n = 16
      }
      
      n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");

      var result = 0,
          length = self.length,
          i = 0;

      for (; i < length; i++) {
        result += self.charCodeAt(i);
      }

      if (n <= 0) {
        return result;
      }

      return result & (Math.pow(2, n) - 1);
    ;
    };

    def.$swapcase = function() {
      var self = this;

      
      var str = self.replace(/([a-z]+)|([A-Z]+)/g, function($0,$1,$2) {
        return $1 ? $0.toUpperCase() : $0.toLowerCase();
      });

      if (self.constructor === String) {
        return str;
      }

      return self.$class().$new(str);
    
    };

    Opal.defn(self, '$swapcase!', def['$<<']);

    def.$to_f = function() {
      var self = this;

      
      if (self.charAt(0) === '_') {
        return 0;
      }

      var result = parseFloat(self.replace(/_/g, ''));

      if (isNaN(result) || result == Infinity || result == -Infinity) {
        return 0;
      }
      else {
        return result;
      }
    
    };

    def.$to_i = function(base) {
      var self = this;

      if (base == null) {
        base = 10
      }
      
      var result,
          string = self.toLowerCase(),
          radix = $scope.get('Opal').$coerce_to(base, $scope.get('Integer'), "to_int");

      if (radix === 1 || radix < 0 || radix > 36) {
        self.$raise($scope.get('ArgumentError'), "invalid radix " + (radix))
      }

      if (/^\s*_/.test(string)) {
        return 0;
      }

      string = string.replace(/^(\s*[+-]?)(0[bodx]?)(.+)$/, function (original, head, flag, tail) {
        switch (tail.charAt(0)) {
        case '+':
        case '-':
          return original;
        case '0':
          if (tail.charAt(1) === 'x' && flag === '0x' && (radix === 0 || radix === 16)) {
            return original;
          }
        }
        switch (flag) {
        case '0b':
          if (radix === 0 || radix === 2) {
            radix = 2;
            return head + tail;
          }
          break;
        case '0':
        case '0o':
          if (radix === 0 || radix === 8) {
            radix = 8;
            return head + tail;
          }
          break;
        case '0d':
          if (radix === 0 || radix === 10) {
            radix = 10;
            return head + tail;
          }
          break;
        case '0x':
          if (radix === 0 || radix === 16) {
            radix = 16;
            return head + tail;
          }
          break;
        }
        return original
      });

      result = parseInt(string.replace(/_(?!_)/g, ''), radix);
      return isNaN(result) ? 0 : result;
    ;
    };

    def.$to_proc = function() {
      var $a, $b, TMP_9, self = this, sym = nil;

      sym = self;
      return ($a = ($b = self).$proc, $a.$$p = (TMP_9 = function(args){var self = TMP_9.$$s || this, block, $a, $b, obj = nil;
args = $slice.call(arguments, 0);
        block = TMP_9.$$p || nil, TMP_9.$$p = null;
      if ((($a = args['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "no receiver given")};
        obj = args.$shift();
        return ($a = ($b = obj).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [sym].concat(args));}, TMP_9.$$s = self, TMP_9), $a).call($b);
    };

    def.$to_s = function() {
      var self = this;

      return self.toString();
    };

    Opal.defn(self, '$to_str', def.$to_s);

    Opal.defn(self, '$to_sym', def.$intern);

    def.$tr = function(from, to) {
      var self = this;

      from = $scope.get('Opal').$coerce_to(from, $scope.get('String'), "to_str").$to_s();
      to = $scope.get('Opal').$coerce_to(to, $scope.get('String'), "to_str").$to_s();
      
      if (from.length == 0 || from === to) {
        return self;
      }

      var subs = {};
      var from_chars = from.split('');
      var from_length = from_chars.length;
      var to_chars = to.split('');
      var to_length = to_chars.length;

      var inverse = false;
      var global_sub = null;
      if (from_chars[0] === '^' && from_chars.length > 1) {
        inverse = true;
        from_chars.shift();
        global_sub = to_chars[to_length - 1]
        from_length -= 1;
      }

      var from_chars_expanded = [];
      var last_from = null;
      var in_range = false;
      for (var i = 0; i < from_length; i++) {
        var ch = from_chars[i];
        if (last_from == null) {
          last_from = ch;
          from_chars_expanded.push(ch);
        }
        else if (ch === '-') {
          if (last_from === '-') {
            from_chars_expanded.push('-');
            from_chars_expanded.push('-');
          }
          else if (i == from_length - 1) {
            from_chars_expanded.push('-');
          }
          else {
            in_range = true;
          }
        }
        else if (in_range) {
          var start = last_from.charCodeAt(0);
          var end = ch.charCodeAt(0);
          if (start > end) {
            self.$raise($scope.get('ArgumentError'), "invalid range \"" + (String.fromCharCode(start)) + "-" + (String.fromCharCode(end)) + "\" in string transliteration")
          }
          for (var c = start + 1; c < end; c++) {
            from_chars_expanded.push(String.fromCharCode(c));
          }
          from_chars_expanded.push(ch);
          in_range = null;
          last_from = null;
        }
        else {
          from_chars_expanded.push(ch);
        }
      }

      from_chars = from_chars_expanded;
      from_length = from_chars.length;

      if (inverse) {
        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = true;
        }
      }
      else {
        if (to_length > 0) {
          var to_chars_expanded = [];
          var last_to = null;
          var in_range = false;
          for (var i = 0; i < to_length; i++) {
            var ch = to_chars[i];
            if (last_from == null) {
              last_from = ch;
              to_chars_expanded.push(ch);
            }
            else if (ch === '-') {
              if (last_to === '-') {
                to_chars_expanded.push('-');
                to_chars_expanded.push('-');
              }
              else if (i == to_length - 1) {
                to_chars_expanded.push('-');
              }
              else {
                in_range = true;
              }
            }
            else if (in_range) {
              var start = last_from.charCodeAt(0);
              var end = ch.charCodeAt(0);
              if (start > end) {
                self.$raise($scope.get('ArgumentError'), "invalid range \"" + (String.fromCharCode(start)) + "-" + (String.fromCharCode(end)) + "\" in string transliteration")
              }
              for (var c = start + 1; c < end; c++) {
                to_chars_expanded.push(String.fromCharCode(c));
              }
              to_chars_expanded.push(ch);
              in_range = null;
              last_from = null;
            }
            else {
              to_chars_expanded.push(ch);
            }
          }

          to_chars = to_chars_expanded;
          to_length = to_chars.length;
        }

        var length_diff = from_length - to_length;
        if (length_diff > 0) {
          var pad_char = (to_length > 0 ? to_chars[to_length - 1] : '');
          for (var i = 0; i < length_diff; i++) {
            to_chars.push(pad_char);
          }
        }

        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = to_chars[i];
        }
      }

      var new_str = ''
      for (var i = 0, length = self.length; i < length; i++) {
        var ch = self.charAt(i);
        var sub = subs[ch];
        if (inverse) {
          new_str += (sub == null ? global_sub : ch);
        }
        else {
          new_str += (sub != null ? sub : ch);
        }
      }
      return new_str;
    
    };

    Opal.defn(self, '$tr!', def['$<<']);

    def.$tr_s = function(from, to) {
      var self = this;

      from = $scope.get('Opal').$coerce_to(from, $scope.get('String'), "to_str").$to_s();
      to = $scope.get('Opal').$coerce_to(to, $scope.get('String'), "to_str").$to_s();
      
      if (from.length == 0) {
        return self;
      }

      var subs = {};
      var from_chars = from.split('');
      var from_length = from_chars.length;
      var to_chars = to.split('');
      var to_length = to_chars.length;

      var inverse = false;
      var global_sub = null;
      if (from_chars[0] === '^' && from_chars.length > 1) {
        inverse = true;
        from_chars.shift();
        global_sub = to_chars[to_length - 1]
        from_length -= 1;
      }

      var from_chars_expanded = [];
      var last_from = null;
      var in_range = false;
      for (var i = 0; i < from_length; i++) {
        var ch = from_chars[i];
        if (last_from == null) {
          last_from = ch;
          from_chars_expanded.push(ch);
        }
        else if (ch === '-') {
          if (last_from === '-') {
            from_chars_expanded.push('-');
            from_chars_expanded.push('-');
          }
          else if (i == from_length - 1) {
            from_chars_expanded.push('-');
          }
          else {
            in_range = true;
          }
        }
        else if (in_range) {
          var start = last_from.charCodeAt(0);
          var end = ch.charCodeAt(0);
          if (start > end) {
            self.$raise($scope.get('ArgumentError'), "invalid range \"" + (String.fromCharCode(start)) + "-" + (String.fromCharCode(end)) + "\" in string transliteration")
          }
          for (var c = start + 1; c < end; c++) {
            from_chars_expanded.push(String.fromCharCode(c));
          }
          from_chars_expanded.push(ch);
          in_range = null;
          last_from = null;
        }
        else {
          from_chars_expanded.push(ch);
        }
      }

      from_chars = from_chars_expanded;
      from_length = from_chars.length;

      if (inverse) {
        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = true;
        }
      }
      else {
        if (to_length > 0) {
          var to_chars_expanded = [];
          var last_to = null;
          var in_range = false;
          for (var i = 0; i < to_length; i++) {
            var ch = to_chars[i];
            if (last_from == null) {
              last_from = ch;
              to_chars_expanded.push(ch);
            }
            else if (ch === '-') {
              if (last_to === '-') {
                to_chars_expanded.push('-');
                to_chars_expanded.push('-');
              }
              else if (i == to_length - 1) {
                to_chars_expanded.push('-');
              }
              else {
                in_range = true;
              }
            }
            else if (in_range) {
              var start = last_from.charCodeAt(0);
              var end = ch.charCodeAt(0);
              if (start > end) {
                self.$raise($scope.get('ArgumentError'), "invalid range \"" + (String.fromCharCode(start)) + "-" + (String.fromCharCode(end)) + "\" in string transliteration")
              }
              for (var c = start + 1; c < end; c++) {
                to_chars_expanded.push(String.fromCharCode(c));
              }
              to_chars_expanded.push(ch);
              in_range = null;
              last_from = null;
            }
            else {
              to_chars_expanded.push(ch);
            }
          }

          to_chars = to_chars_expanded;
          to_length = to_chars.length;
        }

        var length_diff = from_length - to_length;
        if (length_diff > 0) {
          var pad_char = (to_length > 0 ? to_chars[to_length - 1] : '');
          for (var i = 0; i < length_diff; i++) {
            to_chars.push(pad_char);
          }
        }

        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = to_chars[i];
        }
      }
      var new_str = ''
      var last_substitute = null
      for (var i = 0, length = self.length; i < length; i++) {
        var ch = self.charAt(i);
        var sub = subs[ch]
        if (inverse) {
          if (sub == null) {
            if (last_substitute == null) {
              new_str += global_sub;
              last_substitute = true;
            }
          }
          else {
            new_str += ch;
            last_substitute = null;
          }
        }
        else {
          if (sub != null) {
            if (last_substitute == null || last_substitute !== sub) {
              new_str += sub;
              last_substitute = sub;
            }
          }
          else {
            new_str += ch;
            last_substitute = null;
          }
        }
      }
      return new_str;
    
    };

    Opal.defn(self, '$tr_s!', def['$<<']);

    def.$upcase = function() {
      var self = this;

      return self.toUpperCase();
    };

    Opal.defn(self, '$upcase!', def['$<<']);

    def.$freeze = function() {
      var self = this;

      return self;
    };

    def['$frozen?'] = function() {
      var self = this;

      return true;
    };

    def.$upto = TMP_10 = function(stop, excl) {
      var self = this, $iter = TMP_10.$$p, block = $iter || nil;

      if (excl == null) {
        excl = false
      }
      TMP_10.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("upto", stop, excl)
      };
      stop = $scope.get('Opal').$coerce_to(stop, $scope.get('String'), "to_str");
      
      var a, b, s = self.toString();

      if (s.length === 1 && stop.length === 1) {

        a = s.charCodeAt(0);
        b = stop.charCodeAt(0);

        while (a <= b) {
          if (excl && a === b) {
            break;
          }
          block(String.fromCharCode(a));
          a += 1;
        }

      } else if (parseInt(s).toString() === s && parseInt(stop).toString() === stop) {

        a = parseInt(s);
        b = parseInt(stop);

        while (a <= b) {
          if (excl && a === b) {
            break;
          }
          block(a.toString());
          a += 1;
        }

      } else {

        while (s.length <= stop.length && s <= stop) {
          if (excl && s === stop) {
            break;
          }
          block(s);
          s = (s).$succ();
        }

      }
      return self;
    
    };

    
    function char_class_from_char_sets(sets) {
      function explode_sequences_in_character_set(set) {
        var result = '',
            i, len = set.length,
            curr_char,
            skip_next_dash,
            char_code_from,
            char_code_upto,
            char_code;
        for (i = 0; i < len; i++) {
          curr_char = set.charAt(i);
          if (curr_char === '-' && i > 0 && i < (len - 1) && !skip_next_dash) {
            char_code_from = set.charCodeAt(i - 1);
            char_code_upto = set.charCodeAt(i + 1);
            if (char_code_from > char_code_upto) {
              self.$raise($scope.get('ArgumentError'), "invalid range \"" + (char_code_from) + "-" + (char_code_upto) + "\" in string transliteration")
            }
            for (char_code = char_code_from + 1; char_code < char_code_upto + 1; char_code++) {
              result += String.fromCharCode(char_code);
            }
            skip_next_dash = true;
            i++;
          } else {
            skip_next_dash = (curr_char === '\\');
            result += curr_char;
          }
        }
        return result;
      }

      function intersection(setA, setB) {
        if (setA.length === 0) {
          return setB;
        }
        var result = '',
            i, len = setA.length,
            chr;
        for (i = 0; i < len; i++) {
          chr = setA.charAt(i);
          if (setB.indexOf(chr) !== -1) {
            result += chr;
          }
        }
        return result;
      }

      var i, len, set, neg, chr, tmp,
          pos_intersection = '',
          neg_intersection = '';

      for (i = 0, len = sets.length; i < len; i++) {
        set = $scope.get('Opal').$coerce_to(sets[i], $scope.get('String'), "to_str");
        neg = (set.charAt(0) === '^' && set.length > 1);
        set = explode_sequences_in_character_set(neg ? set.slice(1) : set);
        if (neg) {
          neg_intersection = intersection(neg_intersection, set);
        } else {
          pos_intersection = intersection(pos_intersection, set);
        }
      }

      if (pos_intersection.length > 0 && neg_intersection.length > 0) {
        tmp = '';
        for (i = 0, len = pos_intersection.length; i < len; i++) {
          chr = pos_intersection.charAt(i);
          if (neg_intersection.indexOf(chr) === -1) {
            tmp += chr;
          }
        }
        pos_intersection = tmp;
        neg_intersection = '';
      }

      if (pos_intersection.length > 0) {
        return '[' + $scope.get('Regexp').$escape(pos_intersection) + ']';
      }

      if (neg_intersection.length > 0) {
        return '[^' + $scope.get('Regexp').$escape(neg_intersection) + ']';
      }

      return null;
    }
  
  })(self, null);
  return Opal.cdecl($scope, 'Symbol', $scope.get('String'));
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/string/inheritance"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  function $rb_times(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs * rhs : lhs['$*'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$new', '$allocate', '$initialize', '$to_proc', '$__send__', '$class', '$clone', '$respond_to?', '$==', '$inspect', '$map', '$split', '$enum_for', '$each_line', '$to_a', '$%']);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defs(self, '$inherited', function(klass) {
      var self = this, replace = nil;

      replace = $scope.get('Class').$new((($scope.get('String')).$$scope.get('Wrapper')));
      
      klass.$$proto         = replace.$$proto;
      klass.$$proto.$$class = klass;
      klass.$$alloc         = replace.$$alloc;
      klass.$$parent        = (($scope.get('String')).$$scope.get('Wrapper'));

      klass.$allocate = replace.$allocate;
      klass.$new      = replace.$new;
    
    }), nil) && 'inherited'
  })(self, null);
  return (function($base, $super) {
    function $Wrapper(){};
    var self = $Wrapper = $klass($base, $super, 'Wrapper', $Wrapper);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_6, TMP_8;

    def.literal = nil;
    def.$$is_string = true;

    Opal.defs(self, '$allocate', TMP_1 = function(string) {
      var self = this, $iter = TMP_1.$$p, $yield = $iter || nil, obj = nil;

      if (string == null) {
        string = ""
      }
      TMP_1.$$p = null;
      obj = Opal.find_super_dispatcher(self, 'allocate', TMP_1, null, $Wrapper).apply(self, []);
      obj.literal = string;
      return obj;
    });

    Opal.defs(self, '$new', TMP_2 = function(args) {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil, obj = nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      obj = self.$allocate();
      ($a = ($b = obj).$initialize, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      return obj;
    });

    Opal.defs(self, '$[]', function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      return self.$allocate(objects);
    });

    def.$initialize = function(string) {
      var self = this;

      if (string == null) {
        string = ""
      }
      return self.literal = string;
    };

    def.$method_missing = TMP_3 = function(args) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil, result = nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      result = ($a = ($b = self.literal).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      if ((($a = result.$$is_string != null) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = result == self.literal) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self
          } else {
          return self.$class().$allocate(result)
        }
        } else {
        return result
      };
    };

    def.$initialize_copy = function(other) {
      var self = this;

      return self.literal = (other.literal).$clone();
    };

    def['$respond_to?'] = TMP_4 = function(name) {var $zuper = $slice.call(arguments, 0);
      var $a, self = this, $iter = TMP_4.$$p, $yield = $iter || nil;

      TMP_4.$$p = null;
      return ((($a = Opal.find_super_dispatcher(self, 'respond_to?', TMP_4, $iter).apply(self, $zuper)) !== false && $a !== nil) ? $a : self.literal['$respond_to?'](name));
    };

    def['$=='] = function(other) {
      var self = this;

      return self.literal['$=='](other);
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$===', def['$==']);

    def.$to_s = function() {
      var self = this;

      return self.literal;
    };

    Opal.defn(self, '$to_str', def.$to_s);

    def.$inspect = function() {
      var self = this;

      return self.literal.$inspect();
    };

    def['$+'] = function(other) {
      var self = this;

      return $rb_plus(self.literal, other);
    };

    def['$*'] = function(other) {
      var self = this;

      
      var result = $rb_times(self.literal, other);

      if (result.$$is_string) {
        return self.$class().$allocate(result)
      }
      else {
        return result;
      }
    ;
    };

    def.$split = function(pattern, limit) {
      var $a, $b, TMP_5, self = this;

      return ($a = ($b = self.literal.$split(pattern, limit)).$map, $a.$$p = (TMP_5 = function(str){var self = TMP_5.$$s || this;
if (str == null) str = nil;
      return self.$class().$allocate(str)}, TMP_5.$$s = self, TMP_5), $a).call($b);
    };

    def.$replace = function(string) {
      var self = this;

      return self.literal = string;
    };

    def.$each_line = TMP_6 = function(separator) {
      var $a, $b, TMP_7, self = this, $iter = TMP_6.$$p, $yield = $iter || nil;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      TMP_6.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each_line", separator)
      };
      return ($a = ($b = self.literal).$each_line, $a.$$p = (TMP_7 = function(str){var self = TMP_7.$$s || this, $a;
if (str == null) str = nil;
      return $a = Opal.yield1($yield, self.$class().$allocate(str)), $a === $breaker ? $a : $a}, TMP_7.$$s = self, TMP_7), $a).call($b, separator);
    };

    def.$lines = TMP_8 = function(separator) {
      var $a, $b, self = this, $iter = TMP_8.$$p, block = $iter || nil, e = nil;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      TMP_8.$$p = null;
      e = ($a = ($b = self).$each_line, $a.$$p = block.$to_proc(), $a).call($b, separator);
      if (block !== false && block !== nil) {
        return self
        } else {
        return e.$to_a()
      };
    };

    return (def['$%'] = function(data) {
      var self = this;

      return self.literal['$%'](data);
    }, nil) && '%';
  })($scope.get('String'), null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/match_data"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$attr_reader', '$[]', '$raise', '$===', '$inspect', '$to_a', '$coerce_to!']);
  return (function($base, $super) {
    function $MatchData(){};
    var self = $MatchData = $klass($base, $super, 'MatchData', $MatchData);

    var def = self.$$proto, $scope = self.$$scope;

    def.matches = nil;
    self.$attr_reader("post_match", "pre_match", "regexp", "string");

    def.$initialize = function(regexp, match_groups) {
      var self = this;

      $gvars["~"] = self;
      self.regexp = regexp;
      self.begin = match_groups.index;
      self.string = match_groups.input;
      self.pre_match = match_groups.input.slice(0, match_groups.index);
      self.post_match = match_groups.input.slice(match_groups.index + match_groups[0].length);
      self.matches = [];
      
      for (var i = 0, length = match_groups.length; i < length; i++) {
        var group = match_groups[i];

        if (group == null) {
          self.matches.push(nil);
        }
        else {
          self.matches.push(group);
        }
      }
    
    };

    def['$[]'] = function(args) {
      var $a, self = this;

      args = $slice.call(arguments, 0);
      return ($a = self.matches)['$[]'].apply($a, [].concat(args));
    };

    def.$offset = function(n) {
      var self = this;

      
      if (n !== 0) {
        self.$raise($scope.get('ArgumentError'), "MatchData#offset only supports 0th element")
      }
      return [self.begin, self.begin + self.matches[n].length];
    ;
    };

    def['$=='] = function(other) {
      var $a, $b, $c, $d, self = this;

      if ((($a = $scope.get('MatchData')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return ($a = ($b = ($c = ($d = self.string == other.string, $d !== false && $d !== nil ?self.regexp.toString() == other.regexp.toString() : $d), $c !== false && $c !== nil ?self.pre_match == other.pre_match : $c), $b !== false && $b !== nil ?self.post_match == other.post_match : $b), $a !== false && $a !== nil ?self.begin == other.begin : $a);
    };

    Opal.defn(self, '$eql?', def['$==']);

    def.$begin = function(n) {
      var self = this;

      
      if (n !== 0) {
        self.$raise($scope.get('ArgumentError'), "MatchData#begin only supports 0th element")
      }
      return self.begin;
    ;
    };

    def.$end = function(n) {
      var self = this;

      
      if (n !== 0) {
        self.$raise($scope.get('ArgumentError'), "MatchData#end only supports 0th element")
      }
      return self.begin + self.matches[n].length;
    ;
    };

    def.$captures = function() {
      var self = this;

      return self.matches.slice(1);
    };

    def.$inspect = function() {
      var self = this;

      
      var str = "#<MatchData " + (self.matches[0]).$inspect();

      for (var i = 1, length = self.matches.length; i < length; i++) {
        str += " " + i + ":" + (self.matches[i]).$inspect();
      }

      return str + ">";
    ;
    };

    def.$length = function() {
      var self = this;

      return self.matches.length;
    };

    Opal.defn(self, '$size', def.$length);

    def.$to_a = function() {
      var self = this;

      return self.matches;
    };

    def.$to_s = function() {
      var self = this;

      return self.matches[0];
    };

    return (def.$values_at = function(args) {
      var self = this;

      args = $slice.call(arguments, 0);
      
      var i, a, index, values = [];

      for (i = 0; i < args.length; i++) {

        if (args[i].$$is_range) {
          a = (args[i]).$to_a();
          a.unshift(i, 1);
          Array.prototype.splice.apply(args, a);
        }

        index = $scope.get('Opal')['$coerce_to!'](args[i], $scope.get('Integer'), "to_int");

        if (index < 0) {
          index += self.matches.length;
          if (index < 0) {
            values.push(nil);
            continue;
          }
        }

        values.push(self.matches[index]);
      }

      return values;
    
    }, nil) && 'values_at';
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/numeric"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_minus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs - rhs : lhs['$-'](rhs);
  }
  function $rb_lt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs < rhs : lhs['$<'](rhs);
  }
  function $rb_gt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs > rhs : lhs['$>'](rhs);
  }
  function $rb_divide(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs / rhs : lhs['$/'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$coerce', '$===', '$raise', '$class', '$__send__', '$send_coerced', '$coerce_to!', '$-@', '$**', '$respond_to?', '$==', '$enum_for', '$gcd', '$lcm', '$floor', '$%']);
  self.$require("corelib/comparable");
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6;

    self.$include($scope.get('Comparable'));

    def.$$is_number = true;

    def.$__id__ = function() {
      var self = this;

      return (self * 2) + 1;
    };

    Opal.defn(self, '$object_id', def.$__id__);

    def.$coerce = function(other, type) {
      var self = this, $case = nil;

      if (type == null) {
        type = "operation"
      }
      try {
      
      if (other.$$is_number) {
        return [self, other];
      }
      else {
        return other.$coerce(self);
      }
    
      } catch ($err) {if (true) {
        return (function() {$case = type;if ("operation"['$===']($case)) {return self.$raise($scope.get('TypeError'), "" + (other.$class()) + " can't be coerced into Numeric")}else if ("comparison"['$===']($case)) {return self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")}else { return nil }})()
        }else { throw $err; }
      };
    };

    def.$send_coerced = function(method, other) {
      var $a, self = this, type = nil, $case = nil, a = nil, b = nil;

      type = (function() {$case = method;if ("+"['$===']($case) || "-"['$===']($case) || "*"['$===']($case) || "/"['$===']($case) || "%"['$===']($case) || "&"['$===']($case) || "|"['$===']($case) || "^"['$===']($case) || "**"['$===']($case)) {return "operation"}else if (">"['$===']($case) || ">="['$===']($case) || "<"['$===']($case) || "<="['$===']($case) || "<=>"['$===']($case)) {return "comparison"}else { return nil }})();
      $a = Opal.to_ary(self.$coerce(other, type)), a = ($a[0] == null ? nil : $a[0]), b = ($a[1] == null ? nil : $a[1]);
      return a.$__send__(method, b);
    };

    def['$+'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self + other;
      }
      else {
        return self.$send_coerced("+", other);
      }
    
    };

    def['$-'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self - other;
      }
      else {
        return self.$send_coerced("-", other);
      }
    
    };

    def['$*'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self * other;
      }
      else {
        return self.$send_coerced("*", other);
      }
    
    };

    def['$/'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self / other;
      }
      else {
        return self.$send_coerced("/", other);
      }
    
    };

    def['$%'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        if (other < 0 || self < 0) {
          return (self % other + other) % other;
        }
        else {
          return self % other;
        }
      }
      else {
        return self.$send_coerced("%", other);
      }
    
    };

    def['$&'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self & other;
      }
      else {
        return self.$send_coerced("&", other);
      }
    
    };

    def['$|'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self | other;
      }
      else {
        return self.$send_coerced("|", other);
      }
    
    };

    def['$^'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self ^ other;
      }
      else {
        return self.$send_coerced("^", other);
      }
    
    };

    def['$<'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self < other;
      }
      else {
        return self.$send_coerced("<", other);
      }
    
    };

    def['$<='] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self <= other;
      }
      else {
        return self.$send_coerced("<=", other);
      }
    
    };

    def['$>'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self > other;
      }
      else {
        return self.$send_coerced(">", other);
      }
    
    };

    def['$>='] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self >= other;
      }
      else {
        return self.$send_coerced(">=", other);
      }
    
    };

    def['$<=>'] = function(other) {
      var self = this;

      try {
      
      if (other.$$is_number) {
        return self > other ? 1 : (self < other ? -1 : 0);
      }
      else {
        return self.$send_coerced("<=>", other);
      }
    
      } catch ($err) {if (Opal.rescue($err, [$scope.get('ArgumentError')])) {
        return nil
        }else { throw $err; }
      };
    };

    def['$<<'] = function(count) {
      var self = this;

      count = $scope.get('Opal')['$coerce_to!'](count, $scope.get('Integer'), "to_int");
      return count > 0 ? self << count : self >> -count;
    };

    def['$>>'] = function(count) {
      var self = this;

      count = $scope.get('Opal')['$coerce_to!'](count, $scope.get('Integer'), "to_int");
      return count > 0 ? self >> count : self << -count;
    };

    def['$[]'] = function(bit) {
      var self = this, min = nil, max = nil;

      bit = $scope.get('Opal')['$coerce_to!'](bit, $scope.get('Integer'), "to_int");
      min = ((2)['$**'](30))['$-@']();
      max = $rb_minus(((2)['$**'](30)), 1);
      return (bit < min || bit > max) ? 0 : (self >> bit) % 2;
    };

    def['$+@'] = function() {
      var self = this;

      return +self;
    };

    def['$-@'] = function() {
      var self = this;

      return -self;
    };

    def['$~'] = function() {
      var self = this;

      return ~self;
    };

    def['$**'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return Math.pow(self, other);
      }
      else {
        return self.$send_coerced("**", other);
      }
    
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self == Number(other);
      }
      else if (other['$respond_to?']("==")) {
        return other['$=='](self);
      }
      else {
        return false;
      }
    ;
    };

    def.$abs = function() {
      var self = this;

      return Math.abs(self);
    };

    def.$ceil = function() {
      var self = this;

      return Math.ceil(self);
    };

    def.$chr = function(encoding) {
      var self = this;

      return String.fromCharCode(self);
    };

    def.$conj = function() {
      var self = this;

      return self;
    };

    Opal.defn(self, '$conjugate', def.$conj);

    def.$downto = TMP_1 = function(finish) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("downto", finish)
      };
      
      if (!finish.$$is_number) {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (finish.$class()) + " failed")
      }
      for (var i = self; i >= finish; i--) {
        if (block(i) === $breaker) {
          return $breaker.$v;
        }
      }
    ;
      return self;
    };

    Opal.defn(self, '$eql?', def['$==']);

    def['$equal?'] = function(other) {
      var $a, self = this;

      return ((($a = self['$=='](other)) !== false && $a !== nil) ? $a : isNaN(self) && isNaN(other));
    };

    def['$even?'] = function() {
      var self = this;

      return self % 2 === 0;
    };

    def.$floor = function() {
      var self = this;

      return Math.floor(self);
    };

    def.$gcd = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "not an integer")
      };
      
      var min = Math.abs(self),
          max = Math.abs(other);

      while (min > 0) {
        var tmp = min;

        min = max % min;
        max = tmp;
      }

      return max;
    
    };

    def.$gcdlcm = function(other) {
      var self = this;

      return [self.$gcd(), self.$lcm()];
    };

    def.$hash = function() {
      var self = this;

      return 'Numeric:'+self.toString();
    };

    def['$integer?'] = function() {
      var self = this;

      return self % 1 === 0;
    };

    def['$is_a?'] = TMP_2 = function(klass) {var $zuper = $slice.call(arguments, 0);
      var $a, $b, self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

      TMP_2.$$p = null;
      if ((($a = (($b = klass['$==']($scope.get('Fixnum'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Integer'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Float'))) ? $scope.get('Float')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      return Opal.find_super_dispatcher(self, 'is_a?', TMP_2, $iter).apply(self, $zuper);
    };

    Opal.defn(self, '$kind_of?', def['$is_a?']);

    def['$instance_of?'] = TMP_3 = function(klass) {var $zuper = $slice.call(arguments, 0);
      var $a, $b, self = this, $iter = TMP_3.$$p, $yield = $iter || nil;

      TMP_3.$$p = null;
      if ((($a = (($b = klass['$==']($scope.get('Fixnum'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Integer'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Float'))) ? $scope.get('Float')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      return Opal.find_super_dispatcher(self, 'instance_of?', TMP_3, $iter).apply(self, $zuper);
    };

    def.$lcm = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "not an integer")
      };
      
      if (self == 0 || other == 0) {
        return 0;
      }
      else {
        return Math.abs(self * other / self.$gcd(other));
      }
    
    };

    Opal.defn(self, '$magnitude', def.$abs);

    Opal.defn(self, '$modulo', def['$%']);

    def.$next = function() {
      var self = this;

      return self + 1;
    };

    def['$nonzero?'] = function() {
      var self = this;

      return self == 0 ? nil : self;
    };

    def['$odd?'] = function() {
      var self = this;

      return self % 2 !== 0;
    };

    def.$ord = function() {
      var self = this;

      return self;
    };

    def.$pred = function() {
      var self = this;

      return self - 1;
    };

    def.$round = function(ndigits) {
      var self = this;

      if (ndigits == null) {
        ndigits = 0
      }
      
      var scale = Math.pow(10, ndigits);
      return Math.round(self * scale) / scale;
    
    };

    def.$step = TMP_4 = function(limit, step) {
      var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      if (step == null) {
        step = 1
      }
      TMP_4.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("step", limit, step)
      };
      if ((($a = step == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "step cannot be 0")};
      
      var value = self;

      if (step > 0) {
        while (value <= limit) {
          block(value);
          value += step;
        }
      }
      else {
        while (value >= limit) {
          block(value);
          value += step;
        }
      }
    
      return self;
    };

    Opal.defn(self, '$succ', def.$next);

    def.$times = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("times")
      };
      
      for (var i = 0; i < self; i++) {
        if (block(i) === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def.$to_f = function() {
      var self = this;

      return self;
    };

    def.$to_i = function() {
      var self = this;

      return parseInt(self);
    };

    Opal.defn(self, '$to_int', def.$to_i);

    def.$to_s = function(base) {
      var $a, $b, self = this;

      if (base == null) {
        base = 10
      }
      if ((($a = ((($b = $rb_lt(base, 2)) !== false && $b !== nil) ? $b : $rb_gt(base, 36))) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "base must be between 2 and 36")};
      return self.toString(base);
    };

    Opal.defn(self, '$inspect', def.$to_s);

    def.$divmod = function(rhs) {
      var self = this, q = nil, r = nil;

      q = ($rb_divide(self, rhs)).$floor();
      r = self['$%'](rhs);
      return [q, r];
    };

    def.$upto = TMP_6 = function(finish) {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("upto", finish)
      };
      
      if (!finish.$$is_number) {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (finish.$class()) + " failed")
      }
      for (var i = self; i <= finish; i++) {
        if (block(i) === $breaker) {
          return $breaker.$v;
        }
      }
    ;
      return self;
    };

    def['$zero?'] = function() {
      var self = this;

      return self == 0;
    };

    def.$size = function() {
      var self = this;

      return 4;
    };

    def['$nan?'] = function() {
      var self = this;

      return isNaN(self);
    };

    def['$finite?'] = function() {
      var self = this;

      return self != Infinity && self != -Infinity;
    };

    def['$infinite?'] = function() {
      var self = this;

      
      if (self == Infinity) {
        return +1;
      }
      else if (self == -Infinity) {
        return -1;
      }
      else {
        return nil;
      }
    
    };

    def['$positive?'] = function() {
      var self = this;

      return 1 / self > 0;
    };

    return (def['$negative?'] = function() {
      var self = this;

      return 1 / self < 0;
    }, nil) && 'negative?';
  })(self, null);
  Opal.cdecl($scope, 'Fixnum', $scope.get('Numeric'));
  (function($base, $super) {
    function $Integer(){};
    var self = $Integer = $klass($base, $super, 'Integer', $Integer);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defs(self, '$===', function(other) {
      var self = this;

      
      if (!other.$$is_number) {
        return false;
      }

      return (other % 1) === 0;
    
    }), nil) && '==='
  })(self, $scope.get('Numeric'));
  return (function($base, $super) {
    function $Float(){};
    var self = $Float = $klass($base, $super, 'Float', $Float);

    var def = self.$$proto, $scope = self.$$scope, $a;

    Opal.defs(self, '$===', function(other) {
      var self = this;

      return !!other.$$is_number;
    });

    Opal.cdecl($scope, 'INFINITY', Infinity);

    Opal.cdecl($scope, 'NAN', NaN);

    if ((($a = (typeof(Number.EPSILON) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
      return Opal.cdecl($scope, 'EPSILON', Number.EPSILON)
      } else {
      return Opal.cdecl($scope, 'EPSILON', 2.2204460492503130808472633361816E-16)
    };
  })(self, $scope.get('Numeric'));
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/complex"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  return (function($base, $super) {
    function $Complex(){};
    var self = $Complex = $klass($base, $super, 'Complex', $Complex);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Numeric'))
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/rational"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  return (function($base, $super) {
    function $Rational(){};
    var self = $Rational = $klass($base, $super, 'Rational', $Rational);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Numeric'))
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/proc"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$raise']);
  return (function($base, $super) {
    function $Proc(){};
    var self = $Proc = $klass($base, $super, 'Proc', $Proc);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

    def.$$is_proc = true;

    def.$$is_lambda = false;

    Opal.defs(self, '$new', TMP_1 = function() {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "tried to create a Proc object without a block")
      };
      return block;
    });

    def.$call = TMP_2 = function(args) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      
      if (block !== nil) {
        self.$$p = block;
      }

      var result;

      if (self.$$is_lambda) {
        result = self.apply(null, args);
      }
      else {
        result = Opal.yieldX(self, args);
      }

      if (result === $breaker) {
        return $breaker.$v;
      }

      return result;
    
    };

    Opal.defn(self, '$[]', def.$call);

    def.$to_proc = function() {
      var self = this;

      return self;
    };

    def['$lambda?'] = function() {
      var self = this;

      return !!self.$$is_lambda;
    };

    return (def.$arity = function() {
      var self = this;

      return self.length;
    }, nil) && 'arity';
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/method"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$attr_reader', '$class', '$arity', '$new', '$name']);
  (function($base, $super) {
    function $Method(){};
    var self = $Method = $klass($base, $super, 'Method', $Method);

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    def.method = def.receiver = def.owner = def.name = def.obj = nil;
    self.$attr_reader("owner", "receiver", "name");

    def.$initialize = function(receiver, method, name) {
      var self = this;

      self.receiver = receiver;
      self.owner = receiver.$class();
      self.name = name;
      return self.method = method;
    };

    def.$arity = function() {
      var self = this;

      return self.method.$arity();
    };

    def.$call = TMP_1 = function(args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_1.$$p = null;
      
      self.method.$$p = block;

      return self.method.apply(self.receiver, args);
    ;
    };

    Opal.defn(self, '$[]', def.$call);

    def.$unbind = function() {
      var self = this;

      return $scope.get('UnboundMethod').$new(self.owner, self.method, self.name);
    };

    def.$to_proc = function() {
      var self = this;

      return self.method;
    };

    return (def.$inspect = function() {
      var self = this;

      return "#<Method: " + (self.obj.$class()) + "#" + (self.name) + "}>";
    }, nil) && 'inspect';
  })(self, null);
  return (function($base, $super) {
    function $UnboundMethod(){};
    var self = $UnboundMethod = $klass($base, $super, 'UnboundMethod', $UnboundMethod);

    var def = self.$$proto, $scope = self.$$scope;

    def.method = def.name = def.owner = nil;
    self.$attr_reader("owner", "name");

    def.$initialize = function(owner, method, name) {
      var self = this;

      self.owner = owner;
      self.method = method;
      return self.name = name;
    };

    def.$arity = function() {
      var self = this;

      return self.method.$arity();
    };

    def.$bind = function(object) {
      var self = this;

      return $scope.get('Method').$new(object, self.method, self.name);
    };

    return (def.$inspect = function() {
      var self = this;

      return "#<UnboundMethod: " + (self.owner.$name()) + "#" + (self.name) + ">";
    }, nil) && 'inspect';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/range"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_le(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs <= rhs : lhs['$<='](rhs);
  }
  function $rb_lt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs < rhs : lhs['$<'](rhs);
  }
  function $rb_minus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs - rhs : lhs['$-'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$attr_reader', '$<=>', '$raise', '$include?', '$enum_for', '$succ', '$!', '$==', '$===', '$exclude_end?', '$eql?', '$begin', '$end', '$abs', '$to_i', '$inspect']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Range(){};
    var self = $Range = $klass($base, $super, 'Range', $Range);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3;

    def.begin = def.exclude = def.end = nil;
    self.$include($scope.get('Enumerable'));

    def.$$is_range = true;

    self.$attr_reader("begin", "end");

    def.$initialize = function(first, last, exclude) {
      var $a, self = this;

      if (exclude == null) {
        exclude = false
      }
      if ((($a = first['$<=>'](last)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'))
      };
      self.begin = first;
      self.end = last;
      return self.exclude = exclude;
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (!other.$$is_range) {
        return false;
      }

      return self.exclude === other.exclude &&
             self.begin   ==  other.begin &&
             self.end     ==  other.end;
    
    };

    def['$==='] = function(value) {
      var self = this;

      return self['$include?'](value);
    };

    def['$cover?'] = function(value) {
      var $a, $b, self = this;

      return (($a = $rb_le(self.begin, value)) ? ((function() {if ((($b = self.exclude) !== nil && (!$b.$$is_boolean || $b == true))) {
        return $rb_lt(value, self.end)
        } else {
        return $rb_le(value, self.end)
      }; return nil; })()) : $a);
    };

    def.$each = TMP_1 = function() {
      var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil, current = nil, last = nil;

      TMP_1.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each")
      };
      current = self.begin;
      last = self.end;
      while ($rb_lt(current, last)) {
      if (Opal.yield1(block, current) === $breaker) return $breaker.$v;
      current = current.$succ();};
      if ((($a = ($b = self.exclude['$!'](), $b !== false && $b !== nil ?current['$=='](last) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if (Opal.yield1(block, current) === $breaker) return $breaker.$v};
      return self;
    };

    def['$eql?'] = function(other) {
      var $a, $b, self = this;

      if ((($a = $scope.get('Range')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return ($a = ($b = self.exclude['$==='](other['$exclude_end?']()), $b !== false && $b !== nil ?self.begin['$eql?'](other.$begin()) : $b), $a !== false && $a !== nil ?self.end['$eql?'](other.$end()) : $a);
    };

    def['$exclude_end?'] = function() {
      var self = this;

      return self.exclude;
    };

    Opal.defn(self, '$first', def.$begin);

    Opal.defn(self, '$include?', def['$cover?']);

    Opal.defn(self, '$last', def.$end);

    def.$max = TMP_2 = function() {var $zuper = $slice.call(arguments, 0);
      var self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

      TMP_2.$$p = null;
      if (($yield !== nil)) {
        return Opal.find_super_dispatcher(self, 'max', TMP_2, $iter).apply(self, $zuper)
        } else {
        return self.exclude ? self.end - 1 : self.end;
      };
    };

    Opal.defn(self, '$member?', def['$cover?']);

    def.$min = TMP_3 = function() {var $zuper = $slice.call(arguments, 0);
      var self = this, $iter = TMP_3.$$p, $yield = $iter || nil;

      TMP_3.$$p = null;
      if (($yield !== nil)) {
        return Opal.find_super_dispatcher(self, 'min', TMP_3, $iter).apply(self, $zuper)
        } else {
        return self.begin
      };
    };

    Opal.defn(self, '$member?', def['$include?']);

    def.$size = function() {
      var $a, $b, self = this, _begin = nil, _end = nil, infinity = nil;

      _begin = self.begin;
      _end = self.end;
      if ((($a = self.exclude) !== nil && (!$a.$$is_boolean || $a == true))) {
        _end = $rb_minus(_end, 1)};
      if ((($a = ($b = $scope.get('Numeric')['$==='](_begin), $b !== false && $b !== nil ?$scope.get('Numeric')['$==='](_end) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return nil
      };
      if ($rb_lt(_end, _begin)) {
        return 0};
      infinity = (($scope.get('Float')).$$scope.get('INFINITY'));
      if ((($a = ((($b = infinity['$=='](_begin.$abs())) !== false && $b !== nil) ? $b : _end.$abs()['$=='](infinity))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return infinity};
      return ((Math.abs(_end - _begin) + 1)).$to_i();
    };

    def.$step = function(n) {
      var self = this;

      if (n == null) {
        n = 1
      }
      return self.$raise($scope.get('NotImplementedError'));
    };

    def.$to_s = function() {
      var self = this;

      return self.begin.$inspect() + (self.exclude ? '...' : '..') + self.end.$inspect();
    };

    return Opal.defn(self, '$inspect', def.$to_s);
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/time"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_le(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs <= rhs : lhs['$<='](rhs);
  }
  function $rb_minus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs - rhs : lhs['$-'](rhs);
  }
  function $rb_divide(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs / rhs : lhs['$/'](rhs);
  }
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$require', '$include', '$kind_of?', '$to_i', '$coerce_to', '$between?', '$raise', '$new', '$compact', '$nil?', '$===', '$<=>', '$to_f', '$strftime', '$is_a?', '$zero?', '$wday', '$utc?', '$warn', '$year', '$mon', '$day', '$yday', '$hour', '$min', '$sec', '$rjust', '$ljust', '$zone', '$to_s', '$[]', '$cweek_cyear', '$month', '$isdst', '$private', '$!', '$==', '$ceil']);
  self.$require("corelib/comparable");
  return (function($base, $super) {
    function $Time(){};
    var self = $Time = $klass($base, $super, 'Time', $Time);

    var def = self.$$proto, $scope = self.$$scope;

    def.tz_offset = nil;
    self.$include($scope.get('Comparable'));

    
    var days_of_week = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
        short_days   = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
        short_months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
        long_months  = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  ;

    Opal.defs(self, '$at', function(seconds, frac) {
      var self = this;

      if (frac == null) {
        frac = 0
      }
      return new Date(seconds * 1000 + frac);
    });

    Opal.defs(self, '$new', function(year, month, day, hour, minute, second, utc_offset) {
      var self = this;

      
      switch (arguments.length) {
        case 1:
          return new Date(year, 0);

        case 2:
          return new Date(year, month - 1);

        case 3:
          return new Date(year, month - 1, day);

        case 4:
          return new Date(year, month - 1, day, hour);

        case 5:
          return new Date(year, month - 1, day, hour, minute);

        case 6:
          return new Date(year, month - 1, day, hour, minute, second);

        case 7:
          return new Date(year, month - 1, day, hour, minute, second);

        default:
          return new Date();
      }
    
    });

    Opal.defs(self, '$local', function(year, month, day, hour, minute, second, millisecond) {
      var $a, self = this;

      if (month == null) {
        month = nil
      }
      if (day == null) {
        day = nil
      }
      if (hour == null) {
        hour = nil
      }
      if (minute == null) {
        minute = nil
      }
      if (second == null) {
        second = nil
      }
      if (millisecond == null) {
        millisecond = nil
      }
      if ((($a = arguments.length === 10) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        var args = $slice.call(arguments).reverse();

        second = args[9];
        minute = args[8];
        hour   = args[7];
        day    = args[6];
        month  = args[5];
        year   = args[4];
      };
      year = (function() {if ((($a = year['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return year.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(year, $scope.get('Integer'), "to_int")
      }; return nil; })();
      month = (function() {if ((($a = month['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return month.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = month) !== false && $a !== nil) ? $a : 1), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = month['$between?'](1, 12)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "month out of range: " + (month))
      };
      day = (function() {if ((($a = day['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return day.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = day) !== false && $a !== nil) ? $a : 1), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = day['$between?'](1, 31)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "day out of range: " + (day))
      };
      hour = (function() {if ((($a = hour['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return hour.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = hour) !== false && $a !== nil) ? $a : 0), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = hour['$between?'](0, 24)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "hour out of range: " + (hour))
      };
      minute = (function() {if ((($a = minute['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return minute.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = minute) !== false && $a !== nil) ? $a : 0), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = minute['$between?'](0, 59)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "minute out of range: " + (minute))
      };
      second = (function() {if ((($a = second['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return second.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = second) !== false && $a !== nil) ? $a : 0), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = second['$between?'](0, 59)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "second out of range: " + (second))
      };
      return ($a = self).$new.apply($a, [].concat([year, month, day, hour, minute, second].$compact()));
    });

    Opal.defs(self, '$gm', function(year, month, day, hour, minute, second, utc_offset) {
      var $a, self = this;

      if ((($a = year['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('TypeError'), "missing year (got nil)")};
      
      if (month > 12 || day > 31 || hour > 24 || minute > 59 || second > 59) {
        self.$raise($scope.get('ArgumentError'));
      }

      var date = new Date(Date.UTC(year, (month || 1) - 1, (day || 1), (hour || 0), (minute || 0), (second || 0)));
      date.tz_offset = 0
      return date;
    ;
    });

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$mktime = self.$$proto.$local;
      return self.$$proto.$utc = self.$$proto.$gm;
    })(self.$singleton_class());

    Opal.defs(self, '$now', function() {
      var self = this;

      return new Date();
    });

    def['$+'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Time')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('TypeError'), "time + time?")};
      other = $scope.get('Opal').$coerce_to(other, $scope.get('Integer'), "to_int");
      
      var result           = new Date(self.getTime() + (other * 1000));
          result.tz_offset = self.tz_offset;

      return result;
    
    };

    def['$-'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Time')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return (self.getTime() - other.getTime()) / 1000};
      other = $scope.get('Opal').$coerce_to(other, $scope.get('Integer'), "to_int");
      
      var result           = new Date(self.getTime() - (other * 1000));
          result.tz_offset = self.tz_offset;

      return result;
    
    };

    def['$<=>'] = function(other) {
      var self = this;

      return self.$to_f()['$<=>'](other.$to_f());
    };

    def['$=='] = function(other) {
      var self = this;

      return self.$to_f() === other.$to_f();
    };

    def.$asctime = function() {
      var self = this;

      return self.$strftime("%a %b %e %H:%M:%S %Y");
    };

    Opal.defn(self, '$ctime', def.$asctime);

    def.$day = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCDate();
      }
      else {
        return self.getDate();
      }
    ;
    };

    def.$yday = function() {
      var self = this;

      
      // http://javascript.about.com/library/bldayyear.htm
      var onejan = new Date(self.getFullYear(), 0, 1);
      return Math.ceil((self - onejan) / 86400000);
    
    };

    def.$isdst = function() {
      var self = this;

      return self.$raise($scope.get('NotImplementedError'));
    };

    def['$eql?'] = function(other) {
      var $a, self = this;

      return ($a = other['$is_a?']($scope.get('Time')), $a !== false && $a !== nil ?(self['$<=>'](other))['$zero?']() : $a);
    };

    def['$friday?'] = function() {
      var self = this;

      return self.$wday() == 5;
    };

    def.$hour = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCHours();
      }
      else {
        return self.getHours();
      }
    ;
    };

    def.$inspect = function() {
      var $a, self = this;

      if ((($a = self['$utc?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$strftime("%Y-%m-%d %H:%M:%S UTC")
        } else {
        return self.$strftime("%Y-%m-%d %H:%M:%S %z")
      };
    };

    Opal.defn(self, '$mday', def.$day);

    def.$min = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCMinutes();
      }
      else {
        return self.getMinutes();
      }
    ;
    };

    def.$mon = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCMonth() + 1;
      }
      else {
        return self.getMonth() + 1;
      }
    ;
    };

    def['$monday?'] = function() {
      var self = this;

      return self.$wday() == 1;
    };

    Opal.defn(self, '$month', def.$mon);

    def['$saturday?'] = function() {
      var self = this;

      return self.$wday() == 6;
    };

    def.$sec = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCSeconds();
      }
      else {
        return self.getSeconds();
      }
    ;
    };

    def.$usec = function() {
      var self = this;

      self.$warn("Microseconds are not supported");
      return 0;
    };

    def.$zone = function() {
      var self = this;

      
      var string = self.toString(),
          result;

      if (string.indexOf('(') == -1) {
        result = string.match(/[A-Z]{3,4}/)[0];
      }
      else {
        result = string.match(/\([^)]+\)/)[0].match(/[A-Z]/g).join('');
      }

      if (result == "GMT" && /(GMT\W*\d{4})/.test(string)) {
        return RegExp.$1;
      }
      else {
        return result;
      }
    
    };

    def.$getgm = function() {
      var self = this;

      
      var result           = new Date(self.getTime());
          result.tz_offset = 0;

      return result;
    
    };

    def['$gmt?'] = function() {
      var self = this;

      return self.tz_offset === 0;
    };

    def.$gmt_offset = function() {
      var self = this;

      return -self.getTimezoneOffset() * 60;
    };

    def.$strftime = function(format) {
      var self = this;

      
      return format.replace(/%([\-_#^0]*:{0,2})(\d+)?([EO]*)(.)/g, function(full, flags, width, _, conv) {
        var result = "",
            width  = parseInt(width),
            zero   = flags.indexOf('0') !== -1,
            pad    = flags.indexOf('-') === -1,
            blank  = flags.indexOf('_') !== -1,
            upcase = flags.indexOf('^') !== -1,
            invert = flags.indexOf('#') !== -1,
            colons = (flags.match(':') || []).length;

        if (zero && blank) {
          if (flags.indexOf('0') < flags.indexOf('_')) {
            zero = false;
          }
          else {
            blank = false;
          }
        }

        switch (conv) {
          case 'Y':
            result += self.$year();
            break;

          case 'C':
            zero    = !blank;
            result += Math.round(self.$year() / 100);
            break;

          case 'y':
            zero    = !blank;
            result += (self.$year() % 100);
            break;

          case 'm':
            zero    = !blank;
            result += self.$mon();
            break;

          case 'B':
            result += long_months[self.$mon() - 1];
            break;

          case 'b':
          case 'h':
            blank   = !zero;
            result += short_months[self.$mon() - 1];
            break;

          case 'd':
            zero    = !blank
            result += self.$day();
            break;

          case 'e':
            blank   = !zero
            result += self.$day();
            break;

          case 'j':
            result += self.$yday();
            break;

          case 'H':
            zero    = !blank;
            result += self.$hour();
            break;

          case 'k':
            blank   = !zero;
            result += self.$hour();
            break;

          case 'I':
            zero    = !blank;
            result += (self.$hour() % 12 || 12);
            break;

          case 'l':
            blank   = !zero;
            result += (self.$hour() % 12 || 12);
            break;

          case 'P':
            result += (self.$hour() >= 12 ? "pm" : "am");
            break;

          case 'p':
            result += (self.$hour() >= 12 ? "PM" : "AM");
            break;

          case 'M':
            zero    = !blank;
            result += self.$min();
            break;

          case 'S':
            zero    = !blank;
            result += self.$sec()
            break;

          case 'L':
            zero    = !blank;
            width   = isNaN(width) ? 3 : width;
            result += self.getMilliseconds();
            break;

          case 'N':
            width   = isNaN(width) ? 9 : width;
            result += (self.getMilliseconds().toString()).$rjust(3, "0");
            result  = (result).$ljust(width, "0");
            break;

          case 'z':
            var offset  = self.getTimezoneOffset(),
                hours   = Math.floor(Math.abs(offset) / 60),
                minutes = Math.abs(offset) % 60;

            result += offset < 0 ? "+" : "-";
            result += hours < 10 ? "0" : "";
            result += hours;

            if (colons > 0) {
              result += ":";
            }

            result += minutes < 10 ? "0" : "";
            result += minutes;

            if (colons > 1) {
              result += ":00";
            }

            break;

          case 'Z':
            result += self.$zone();
            break;

          case 'A':
            result += days_of_week[self.$wday()];
            break;

          case 'a':
            result += short_days[self.$wday()];
            break;

          case 'u':
            result += (self.$wday() + 1);
            break;

          case 'w':
            result += self.$wday();
            break;

          case 'V':
            result += self.$cweek_cyear()['$[]'](0).$to_s().$rjust(2, "0");
            break;

          case 'G':
            result += self.$cweek_cyear()['$[]'](1);
            break;

          case 'g':
            result += self.$cweek_cyear()['$[]'](1)['$[]']($range(-2, -1, false));
            break;

          case 's':
            result += self.$to_i();
            break;

          case 'n':
            result += "\n";
            break;

          case 't':
            result += "\t";
            break;

          case '%':
            result += "%";
            break;

          case 'c':
            result += self.$strftime("%a %b %e %T %Y");
            break;

          case 'D':
          case 'x':
            result += self.$strftime("%m/%d/%y");
            break;

          case 'F':
            result += self.$strftime("%Y-%m-%d");
            break;

          case 'v':
            result += self.$strftime("%e-%^b-%4Y");
            break;

          case 'r':
            result += self.$strftime("%I:%M:%S %p");
            break;

          case 'R':
            result += self.$strftime("%H:%M");
            break;

          case 'T':
          case 'X':
            result += self.$strftime("%H:%M:%S");
            break;

          default:
            return full;
        }

        if (upcase) {
          result = result.toUpperCase();
        }

        if (invert) {
          result = result.replace(/[A-Z]/, function(c) { c.toLowerCase() }).
                          replace(/[a-z]/, function(c) { c.toUpperCase() });
        }

        if (pad && (zero || blank)) {
          result = (result).$rjust(isNaN(width) ? 2 : width, blank ? " " : "0");
        }

        return result;
      });
    
    };

    def['$sunday?'] = function() {
      var self = this;

      return self.$wday() == 0;
    };

    def['$thursday?'] = function() {
      var self = this;

      return self.$wday() == 4;
    };

    def.$to_a = function() {
      var self = this;

      return [self.$sec(), self.$min(), self.$hour(), self.$day(), self.$month(), self.$year(), self.$wday(), self.$yday(), self.$isdst(), self.$zone()];
    };

    def.$to_f = function() {
      var self = this;

      return self.getTime() / 1000;
    };

    def.$to_i = function() {
      var self = this;

      return parseInt(self.getTime() / 1000);
    };

    Opal.defn(self, '$to_s', def.$inspect);

    def['$tuesday?'] = function() {
      var self = this;

      return self.$wday() == 2;
    };

    Opal.defn(self, '$utc?', def['$gmt?']);

    Opal.defn(self, '$utc_offset', def.$gmt_offset);

    def.$wday = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCDay();
      }
      else {
        return self.getDay();
      }
    ;
    };

    def['$wednesday?'] = function() {
      var self = this;

      return self.$wday() == 3;
    };

    def.$year = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCFullYear();
      }
      else {
        return self.getFullYear();
      }
    ;
    };

    self.$private("cweek_cyear");

    return (def.$cweek_cyear = function() {
      var $a, $b, self = this, jan01 = nil, jan01_wday = nil, first_monday = nil, year = nil, offset = nil, week = nil, dec31 = nil, dec31_wday = nil;

      jan01 = $scope.get('Time').$new(self.$year(), 1, 1);
      jan01_wday = jan01.$wday();
      first_monday = 0;
      year = self.$year();
      if ((($a = (($b = $rb_le(jan01_wday, 4)) ? jan01_wday['$=='](0)['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        offset = $rb_minus(jan01_wday, 1)
        } else {
        offset = $rb_minus($rb_minus(jan01_wday, 7), 1);
        if (offset['$=='](-8)) {
          offset = -1};
      };
      week = ($rb_divide(($rb_plus(self.$yday(), offset)), 7.0)).$ceil();
      if ($rb_le(week, 0)) {
        return $scope.get('Time').$new($rb_minus(self.$year(), 1), 12, 31).$cweek_cyear()
      } else if (week['$=='](53)) {
        dec31 = $scope.get('Time').$new(self.$year(), 12, 31);
        dec31_wday = dec31.$wday();
        if ((($a = (($b = $rb_le(dec31_wday, 3)) ? dec31_wday['$=='](0)['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
          week = 1;
          year = $rb_plus(year, 1);};};
      return [week, year];
    }, nil) && 'cweek_cyear';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/struct"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_lt(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs < rhs : lhs['$<'](rhs);
  }
  function $rb_ge(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs >= rhs : lhs['$>='](rhs);
  }
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$include', '$==', '$[]', '$upcase', '$const_set', '$new', '$unshift', '$each', '$define_struct_attribute', '$instance_eval', '$to_proc', '$raise', '$<<', '$members', '$attr_accessor', '$each_with_index', '$instance_variable_set', '$class', '$===', '$-@', '$size', '$include?', '$to_sym', '$instance_variable_get', '$enum_for', '$hash', '$all?', '$length', '$map', '$join', '$inspect', '$each_pair', '$inject', '$[]=', '$flatten', '$to_a']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Struct(){};
    var self = $Struct = $klass($base, $super, 'Struct', $Struct);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_6, TMP_8;

    self.$include($scope.get('Enumerable'));

    Opal.defs(self, '$new', TMP_1 = function(name, args) {var $zuper = $slice.call(arguments, 0);
      var $a, $b, $c, TMP_2, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_1.$$p = null;
      if (self['$==']($scope.get('Struct'))) {
        } else {
        return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Struct).apply(self, $zuper)
      };
      if (name['$[]'](0)['$=='](name['$[]'](0).$upcase())) {
        return $scope.get('Struct').$const_set(name, ($a = self).$new.apply($a, [].concat(args)))
        } else {
        args.$unshift(name);
        return ($b = ($c = $scope.get('Class')).$new, $b.$$p = (TMP_2 = function(){var self = TMP_2.$$s || this, $a, $b, TMP_3, $c;

        ($a = ($b = args).$each, $a.$$p = (TMP_3 = function(arg){var self = TMP_3.$$s || this;
if (arg == null) arg = nil;
          return self.$define_struct_attribute(arg)}, TMP_3.$$s = self, TMP_3), $a).call($b);
          if (block !== false && block !== nil) {
            return ($a = ($c = self).$instance_eval, $a.$$p = block.$to_proc(), $a).call($c)
            } else {
            return nil
          };}, TMP_2.$$s = self, TMP_2), $b).call($c, self);
      };
    });

    Opal.defs(self, '$define_struct_attribute', function(name) {
      var self = this;

      if (self['$==']($scope.get('Struct'))) {
        self.$raise($scope.get('ArgumentError'), "you cannot define attributes to the Struct class")};
      self.$members()['$<<'](name);
      return self.$attr_accessor(name);
    });

    Opal.defs(self, '$members', function() {
      var $a, self = this;
      if (self.members == null) self.members = nil;

      if (self['$==']($scope.get('Struct'))) {
        self.$raise($scope.get('ArgumentError'), "the Struct class has no members")};
      return ((($a = self.members) !== false && $a !== nil) ? $a : self.members = []);
    });

    Opal.defs(self, '$inherited', function(klass) {
      var $a, $b, TMP_4, self = this, members = nil;
      if (self.members == null) self.members = nil;

      members = self.members;
      return ($a = ($b = klass).$instance_eval, $a.$$p = (TMP_4 = function(){var self = TMP_4.$$s || this;

      return self.members = members}, TMP_4.$$s = self, TMP_4), $a).call($b);
    });

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      return self.$$proto['$[]'] = self.$$proto.$new
    })(self.$singleton_class());

    def.$initialize = function(args) {
      var $a, $b, TMP_5, self = this;

      args = $slice.call(arguments, 0);
      return ($a = ($b = self.$members()).$each_with_index, $a.$$p = (TMP_5 = function(name, index){var self = TMP_5.$$s || this;
if (name == null) name = nil;if (index == null) index = nil;
      return self.$instance_variable_set("@" + (name), args['$[]'](index))}, TMP_5.$$s = self, TMP_5), $a).call($b);
    };

    def.$members = function() {
      var self = this;

      return self.$class().$members();
    };

    def['$[]'] = function(name) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ($rb_lt(name, self.$members().$size()['$-@']())) {
          self.$raise($scope.get('IndexError'), "offset " + (name) + " too small for struct(size:" + (self.$members().$size()) + ")")};
        if ($rb_ge(name, self.$members().$size())) {
          self.$raise($scope.get('IndexError'), "offset " + (name) + " too large for struct(size:" + (self.$members().$size()) + ")")};
        name = self.$members()['$[]'](name);
      } else if ((($a = $scope.get('String')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = self.$members()['$include?'](name.$to_sym())) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          self.$raise($scope.get('NameError'), "no member '" + (name) + "' in struct")
        }
        } else {
        self.$raise($scope.get('TypeError'), "no implicit conversion of " + (name.$class()) + " into Integer")
      };
      return self.$instance_variable_get("@" + (name));
    };

    def['$[]='] = function(name, value) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ($rb_lt(name, self.$members().$size()['$-@']())) {
          self.$raise($scope.get('IndexError'), "offset " + (name) + " too small for struct(size:" + (self.$members().$size()) + ")")};
        if ($rb_ge(name, self.$members().$size())) {
          self.$raise($scope.get('IndexError'), "offset " + (name) + " too large for struct(size:" + (self.$members().$size()) + ")")};
        name = self.$members()['$[]'](name);
      } else if ((($a = $scope.get('String')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = self.$members()['$include?'](name.$to_sym())) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          self.$raise($scope.get('NameError'), "no member '" + (name) + "' in struct")
        }
        } else {
        self.$raise($scope.get('TypeError'), "no implicit conversion of " + (name.$class()) + " into Integer")
      };
      return self.$instance_variable_set("@" + (name), value);
    };

    def.$each = TMP_6 = function() {
      var $a, $b, TMP_7, self = this, $iter = TMP_6.$$p, $yield = $iter || nil;

      TMP_6.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each")
      };
      ($a = ($b = self.$members()).$each, $a.$$p = (TMP_7 = function(name){var self = TMP_7.$$s || this, $a;
if (name == null) name = nil;
      return $a = Opal.yield1($yield, self['$[]'](name)), $a === $breaker ? $a : $a}, TMP_7.$$s = self, TMP_7), $a).call($b);
      return self;
    };

    def.$each_pair = TMP_8 = function() {
      var $a, $b, TMP_9, self = this, $iter = TMP_8.$$p, $yield = $iter || nil;

      TMP_8.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each_pair")
      };
      ($a = ($b = self.$members()).$each, $a.$$p = (TMP_9 = function(name){var self = TMP_9.$$s || this, $a;
if (name == null) name = nil;
      return $a = Opal.yieldX($yield, [name, self['$[]'](name)]), $a === $breaker ? $a : $a}, TMP_9.$$s = self, TMP_9), $a).call($b);
      return self;
    };

    def['$eql?'] = function(other) {
      var $a, $b, $c, TMP_10, self = this;

      return ((($a = self.$hash()['$=='](other.$hash())) !== false && $a !== nil) ? $a : ($b = ($c = other.$each_with_index())['$all?'], $b.$$p = (TMP_10 = function(object, index){var self = TMP_10.$$s || this;
if (object == null) object = nil;if (index == null) index = nil;
      return self['$[]'](self.$members()['$[]'](index))['$=='](object)}, TMP_10.$$s = self, TMP_10), $b).call($c));
    };

    def.$length = function() {
      var self = this;

      return self.$members().$length();
    };

    Opal.defn(self, '$size', def.$length);

    def.$to_a = function() {
      var $a, $b, TMP_11, self = this;

      return ($a = ($b = self.$members()).$map, $a.$$p = (TMP_11 = function(name){var self = TMP_11.$$s || this;
if (name == null) name = nil;
      return self['$[]'](name)}, TMP_11.$$s = self, TMP_11), $a).call($b);
    };

    Opal.defn(self, '$values', def.$to_a);

    def.$inspect = function() {
      var $a, $b, TMP_12, self = this, result = nil;

      result = "#<struct ";
      if (self.$class()['$==']($scope.get('Struct'))) {
        result = $rb_plus(result, "" + (self.$class()) + " ")};
      result = $rb_plus(result, ($a = ($b = self.$each_pair()).$map, $a.$$p = (TMP_12 = function(name, value){var self = TMP_12.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
      return "" + (name) + "=" + (value.$inspect())}, TMP_12.$$s = self, TMP_12), $a).call($b).$join(", "));
      result = $rb_plus(result, ">");
      return result;
    };

    Opal.defn(self, '$to_s', def.$inspect);

    def.$to_h = function() {
      var $a, $b, TMP_13, self = this;

      return ($a = ($b = self.$members()).$inject, $a.$$p = (TMP_13 = function(h, name){var self = TMP_13.$$s || this;
if (h == null) h = nil;if (name == null) name = nil;
      h['$[]='](name, self['$[]'](name));
        return h;}, TMP_13.$$s = self, TMP_13), $a).call($b, $hash2([], {}));
    };

    return (def.$values_at = function(args) {
      var $a, $b, TMP_14, self = this;

      args = $slice.call(arguments, 0);
      args = ($a = ($b = args).$map, $a.$$p = (TMP_14 = function(arg){var self = TMP_14.$$s || this;
if (arg == null) arg = nil;
      return arg.$$is_range ? arg.$to_a() : arg;}, TMP_14.$$s = self, TMP_14), $a).call($b).$flatten();
      
      var result = [];
      for (var i = 0, len = args.length; i < len; i++) {
        if (!args[i].$$is_number) {
          self.$raise($scope.get('TypeError'), "no implicit conversion of " + ((args[i]).$class()) + " into Integer")
        }
        result.push(self['$[]'](args[i]));
      }
      return result;
    ;
    }, nil) && 'values_at';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/io"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var $a, $b, self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $module = Opal.module, $gvars = Opal.gvars;
  if ($gvars.stdout == null) $gvars.stdout = nil;
  if ($gvars.stderr == null) $gvars.stderr = nil;

  Opal.add_stubs(['$attr_accessor', '$size', '$write', '$join', '$map', '$String', '$empty?', '$concat', '$chomp', '$getbyte', '$getc', '$raise', '$new', '$write_proc=', '$extend']);
  (function($base, $super) {
    function $IO(){};
    var self = $IO = $klass($base, $super, 'IO', $IO);

    var def = self.$$proto, $scope = self.$$scope;

    def.tty = def.closed = nil;
    Opal.cdecl($scope, 'SEEK_SET', 0);

    Opal.cdecl($scope, 'SEEK_CUR', 1);

    Opal.cdecl($scope, 'SEEK_END', 2);

    def['$tty?'] = function() {
      var self = this;

      return self.tty;
    };

    def['$closed?'] = function() {
      var self = this;

      return self.closed;
    };

    self.$attr_accessor("write_proc");

    def.$write = function(string) {
      var self = this;

      self.write_proc(string);
      return string.$size();
    };

    self.$attr_accessor("sync");

    (function($base) {
      var self = $module($base, 'Writable');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defn(self, '$<<', function(string) {
        var self = this;

        self.$write(string);
        return self;
      });

      Opal.defn(self, '$print', function(args) {
        var $a, $b, TMP_1, self = this;
        if ($gvars[","] == null) $gvars[","] = nil;

        args = $slice.call(arguments, 0);
        self.$write(($a = ($b = args).$map, $a.$$p = (TMP_1 = function(arg){var self = TMP_1.$$s || this;
if (arg == null) arg = nil;
        return self.$String(arg)}, TMP_1.$$s = self, TMP_1), $a).call($b).$join($gvars[","]));
        return nil;
      });

      Opal.defn(self, '$puts', function(args) {
        var $a, $b, TMP_2, self = this, newline = nil;
        if ($gvars["/"] == null) $gvars["/"] = nil;

        args = $slice.call(arguments, 0);
        newline = $gvars["/"];
        if ((($a = args['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$write($gvars["/"])
          } else {
          self.$write(($a = ($b = args).$map, $a.$$p = (TMP_2 = function(arg){var self = TMP_2.$$s || this;
if (arg == null) arg = nil;
          return self.$String(arg).$chomp()}, TMP_2.$$s = self, TMP_2), $a).call($b).$concat([nil]).$join(newline))
        };
        return nil;
      });
    })(self);

    return (function($base) {
      var self = $module($base, 'Readable');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defn(self, '$readbyte', function() {
        var self = this;

        return self.$getbyte();
      });

      Opal.defn(self, '$readchar', function() {
        var self = this;

        return self.$getc();
      });

      Opal.defn(self, '$readline', function(sep) {
        var self = this;
        if ($gvars["/"] == null) $gvars["/"] = nil;

        if (sep == null) {
          sep = $gvars["/"]
        }
        return self.$raise($scope.get('NotImplementedError'));
      });

      Opal.defn(self, '$readpartial', function(integer, outbuf) {
        var self = this;

        if (outbuf == null) {
          outbuf = nil
        }
        return self.$raise($scope.get('NotImplementedError'));
      });
    })(self);
  })(self, null);
  Opal.cdecl($scope, 'STDERR', $gvars.stderr = $scope.get('IO').$new());
  Opal.cdecl($scope, 'STDIN', $gvars.stdin = $scope.get('IO').$new());
  Opal.cdecl($scope, 'STDOUT', $gvars.stdout = $scope.get('IO').$new());
  (($a = [typeof(process) === 'object' ? function(s){process.stdout.write(s)} : function(s){console.log(s)}]), $b = $gvars.stdout, $b['$write_proc='].apply($b, $a), $a[$a.length-1]);
  (($a = [typeof(process) === 'object' ? function(s){process.stderr.write(s)} : function(s){console.warn(s)}]), $b = $gvars.stderr, $b['$write_proc='].apply($b, $a), $a[$a.length-1]);
  $gvars.stdout.$extend((($scope.get('IO')).$$scope.get('Writable')));
  return $gvars.stderr.$extend((($scope.get('IO')).$$scope.get('Writable')));
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/main"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$include']);
  Opal.defs(self, '$to_s', function() {
    var self = this;

    return "main";
  });
  return (Opal.defs(self, '$include', function(mod) {
    var self = this;

    return $scope.get('Object').$include(mod);
  }), nil) && 'include';
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/variables"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $gvars = Opal.gvars, $hash2 = Opal.hash2;

  Opal.add_stubs(['$new']);
  $gvars["&"] = $gvars["~"] = $gvars["`"] = $gvars["'"] = nil;
  $gvars.LOADED_FEATURES = $gvars["\""] = Opal.loaded_features;
  $gvars.LOAD_PATH = $gvars[":"] = [];
  $gvars["/"] = "\n";
  $gvars[","] = nil;
  Opal.cdecl($scope, 'ARGV', []);
  Opal.cdecl($scope, 'ARGF', $scope.get('Object').$new());
  Opal.cdecl($scope, 'ENV', $hash2([], {}));
  $gvars.VERBOSE = false;
  $gvars.DEBUG = false;
  $gvars.SAFE = 0;
  Opal.cdecl($scope, 'RUBY_PLATFORM', "opal");
  Opal.cdecl($scope, 'RUBY_ENGINE', "opal");
  Opal.cdecl($scope, 'RUBY_VERSION', "2.1.5");
  Opal.cdecl($scope, 'RUBY_ENGINE_VERSION', "0.8.1");
  return Opal.cdecl($scope, 'RUBY_RELEASE_DATE', "2015-10-12");
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/dir"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$[]']);
  return (function($base, $super) {
    function $Dir(){};
    var self = $Dir = $klass($base, $super, 'Dir', $Dir);

    var def = self.$$proto, $scope = self.$$scope;

    return (function(self) {
      var $scope = self.$$scope, def = self.$$proto, TMP_1;

      self.$$proto.$chdir = TMP_1 = function(dir) {
        var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil, prev_cwd = nil;

        TMP_1.$$p = null;
        try {
        prev_cwd = Opal.current_dir;
        Opal.current_dir = dir;
        return $a = Opal.yieldX($yield, []), $a === $breaker ? $a : $a;
        } finally {
        Opal.current_dir = prev_cwd;
        };
      };
      self.$$proto.$pwd = function() {
        var self = this;

        return Opal.current_dir || '.';
      };
      self.$$proto.$getwd = self.$$proto.$pwd;
      return (self.$$proto.$home = function() {
        var $a, self = this;

        return ((($a = $scope.get('ENV')['$[]']("HOME")) !== false && $a !== nil) ? $a : ".");
      }, nil) && 'home';
    })(self.$singleton_class())
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["corelib/file"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$join', '$compact', '$split', '$==', '$first', '$[]=', '$home', '$each', '$pop', '$<<', '$[]', '$gsub', '$find', '$=~']);
  return (function($base, $super) {
    function $File(){};
    var self = $File = $klass($base, $super, 'File', $File);

    var def = self.$$proto, $scope = self.$$scope;

    Opal.cdecl($scope, 'Separator', Opal.cdecl($scope, 'SEPARATOR', "/"));

    Opal.cdecl($scope, 'ALT_SEPARATOR', nil);

    Opal.cdecl($scope, 'PATH_SEPARATOR', ":");

    return (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$expand_path = function(path, basedir) {
        var $a, $b, TMP_1, self = this, parts = nil, new_parts = nil;

        if (basedir == null) {
          basedir = nil
        }
        path = [basedir, path].$compact().$join($scope.get('SEPARATOR'));
        parts = path.$split($scope.get('SEPARATOR'));
        new_parts = [];
        if (parts.$first()['$==']("~")) {
          parts['$[]='](0, $scope.get('Dir').$home())};
        ($a = ($b = parts).$each, $a.$$p = (TMP_1 = function(part){var self = TMP_1.$$s || this;
if (part == null) part = nil;
        if (part['$==']("..")) {
            return new_parts.$pop()
            } else {
            return new_parts['$<<'](part)
          }}, TMP_1.$$s = self, TMP_1), $a).call($b);
        return new_parts.$join($scope.get('SEPARATOR'));
      };
      self.$$proto.$dirname = function(path) {
        var self = this;

        return self.$split(path)['$[]']($range(0, -2, false));
      };
      self.$$proto.$basename = function(path) {
        var self = this;

        return self.$split(path)['$[]'](-1);
      };
      self.$$proto['$exist?'] = function(path) {
        var self = this;

        return Opal.modules[path] != null;
      };
      self.$$proto['$exists?'] = self.$$proto['$exist?'];
      self.$$proto['$directory?'] = function(path) {
        var $a, $b, TMP_2, self = this, files = nil, file = nil;

        files = [];
        
        for (var key in Opal.modules) {
          files.push(key)
        }
      ;
        path = path.$gsub((new RegExp("(^." + $scope.get('SEPARATOR') + "+|" + $scope.get('SEPARATOR') + "+$)")));
        file = ($a = ($b = files).$find, $a.$$p = (TMP_2 = function(file){var self = TMP_2.$$s || this;
if (file == null) file = nil;
        return file['$=~']((new RegExp("^" + path)))}, TMP_2.$$s = self, TMP_2), $a).call($b);
        return file;
      };
      self.$$proto.$join = function(paths) {
        var self = this;

        paths = $slice.call(arguments, 0);
        return paths.$join($scope.get('SEPARATOR')).$gsub((new RegExp("" + $scope.get('SEPARATOR') + "+")), $scope.get('SEPARATOR'));
      };
      return (self.$$proto.$split = function(path) {
        var self = this;

        return path.$split($scope.get('SEPARATOR'));
      }, nil) && 'split';
    })(self.$singleton_class());
  })(self, $scope.get('IO'))
};
/* Generated by Opal 0.8.1 */
(function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("corelib/runtime");
  self.$require("corelib/helpers");
  self.$require("corelib/module");
  self.$require("corelib/class");
  self.$require("corelib/basic_object");
  self.$require("corelib/kernel");
  self.$require("corelib/nil_class");
  self.$require("corelib/boolean");
  self.$require("corelib/error");
  self.$require("corelib/regexp");
  self.$require("corelib/comparable");
  self.$require("corelib/enumerable");
  self.$require("corelib/enumerator");
  self.$require("corelib/array");
  self.$require("corelib/array/inheritance");
  self.$require("corelib/hash");
  self.$require("corelib/string");
  self.$require("corelib/string/inheritance");
  self.$require("corelib/match_data");
  self.$require("corelib/numeric");
  self.$require("corelib/complex");
  self.$require("corelib/rational");
  self.$require("corelib/proc");
  self.$require("corelib/method");
  self.$require("corelib/range");
  self.$require("corelib/time");
  self.$require("corelib/struct");
  self.$require("corelib/io");
  self.$require("corelib/main");
  self.$require("corelib/variables");
  self.$require("corelib/dir");
  return self.$require("corelib/file");
})(Opal);
/*!
 * jQuery JavaScript Library v1.12.0
 * http://jquery.com/
 *
 * Includes Sizzle.js
 * http://sizzlejs.com/
 *
 * Copyright jQuery Foundation and other contributors
 * Released under the MIT license
 * http://jquery.org/license
 *
 * Date: 2016-01-08T19:56Z
 */


(function( global, factory ) {

	if ( typeof module === "object" && typeof module.exports === "object" ) {
		// For CommonJS and CommonJS-like environments where a proper `window`
		// is present, execute the factory and get jQuery.
		// For environments that do not have a `window` with a `document`
		// (such as Node.js), expose a factory as module.exports.
		// This accentuates the need for the creation of a real `window`.
		// e.g. var jQuery = require("jquery")(window);
		// See ticket #14549 for more info.
		module.exports = global.document ?
			factory( global, true ) :
			function( w ) {
				if ( !w.document ) {
					throw new Error( "jQuery requires a window with a document" );
				}
				return factory( w );
			};
	} else {
		factory( global );
	}

// Pass this if window is not defined yet
}(typeof window !== "undefined" ? window : this, function( window, noGlobal ) {

// Support: Firefox 18+
// Can't be in strict mode, several libs including ASP.NET trace
// the stack via arguments.caller.callee and Firefox dies if
// you try to trace through "use strict" call chains. (#13335)
//"use strict";
var deletedIds = [];

var document = window.document;

var slice = deletedIds.slice;

var concat = deletedIds.concat;

var push = deletedIds.push;

var indexOf = deletedIds.indexOf;

var class2type = {};

var toString = class2type.toString;

var hasOwn = class2type.hasOwnProperty;

var support = {};



var
	version = "1.12.0",

	// Define a local copy of jQuery
	jQuery = function( selector, context ) {

		// The jQuery object is actually just the init constructor 'enhanced'
		// Need init if jQuery is called (just allow error to be thrown if not included)
		return new jQuery.fn.init( selector, context );
	},

	// Support: Android<4.1, IE<9
	// Make sure we trim BOM and NBSP
	rtrim = /^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g,

	// Matches dashed string for camelizing
	rmsPrefix = /^-ms-/,
	rdashAlpha = /-([\da-z])/gi,

	// Used by jQuery.camelCase as callback to replace()
	fcamelCase = function( all, letter ) {
		return letter.toUpperCase();
	};

jQuery.fn = jQuery.prototype = {

	// The current version of jQuery being used
	jquery: version,

	constructor: jQuery,

	// Start with an empty selector
	selector: "",

	// The default length of a jQuery object is 0
	length: 0,

	toArray: function() {
		return slice.call( this );
	},

	// Get the Nth element in the matched element set OR
	// Get the whole matched element set as a clean array
	get: function( num ) {
		return num != null ?

			// Return just the one element from the set
			( num < 0 ? this[ num + this.length ] : this[ num ] ) :

			// Return all the elements in a clean array
			slice.call( this );
	},

	// Take an array of elements and push it onto the stack
	// (returning the new matched element set)
	pushStack: function( elems ) {

		// Build a new jQuery matched element set
		var ret = jQuery.merge( this.constructor(), elems );

		// Add the old object onto the stack (as a reference)
		ret.prevObject = this;
		ret.context = this.context;

		// Return the newly-formed element set
		return ret;
	},

	// Execute a callback for every element in the matched set.
	each: function( callback ) {
		return jQuery.each( this, callback );
	},

	map: function( callback ) {
		return this.pushStack( jQuery.map( this, function( elem, i ) {
			return callback.call( elem, i, elem );
		} ) );
	},

	slice: function() {
		return this.pushStack( slice.apply( this, arguments ) );
	},

	first: function() {
		return this.eq( 0 );
	},

	last: function() {
		return this.eq( -1 );
	},

	eq: function( i ) {
		var len = this.length,
			j = +i + ( i < 0 ? len : 0 );
		return this.pushStack( j >= 0 && j < len ? [ this[ j ] ] : [] );
	},

	end: function() {
		return this.prevObject || this.constructor();
	},

	// For internal use only.
	// Behaves like an Array's method, not like a jQuery method.
	push: push,
	sort: deletedIds.sort,
	splice: deletedIds.splice
};

jQuery.extend = jQuery.fn.extend = function() {
	var src, copyIsArray, copy, name, options, clone,
		target = arguments[ 0 ] || {},
		i = 1,
		length = arguments.length,
		deep = false;

	// Handle a deep copy situation
	if ( typeof target === "boolean" ) {
		deep = target;

		// skip the boolean and the target
		target = arguments[ i ] || {};
		i++;
	}

	// Handle case when target is a string or something (possible in deep copy)
	if ( typeof target !== "object" && !jQuery.isFunction( target ) ) {
		target = {};
	}

	// extend jQuery itself if only one argument is passed
	if ( i === length ) {
		target = this;
		i--;
	}

	for ( ; i < length; i++ ) {

		// Only deal with non-null/undefined values
		if ( ( options = arguments[ i ] ) != null ) {

			// Extend the base object
			for ( name in options ) {
				src = target[ name ];
				copy = options[ name ];

				// Prevent never-ending loop
				if ( target === copy ) {
					continue;
				}

				// Recurse if we're merging plain objects or arrays
				if ( deep && copy && ( jQuery.isPlainObject( copy ) ||
					( copyIsArray = jQuery.isArray( copy ) ) ) ) {

					if ( copyIsArray ) {
						copyIsArray = false;
						clone = src && jQuery.isArray( src ) ? src : [];

					} else {
						clone = src && jQuery.isPlainObject( src ) ? src : {};
					}

					// Never move original objects, clone them
					target[ name ] = jQuery.extend( deep, clone, copy );

				// Don't bring in undefined values
				} else if ( copy !== undefined ) {
					target[ name ] = copy;
				}
			}
		}
	}

	// Return the modified object
	return target;
};

jQuery.extend( {

	// Unique for each copy of jQuery on the page
	expando: "jQuery" + ( version + Math.random() ).replace( /\D/g, "" ),

	// Assume jQuery is ready without the ready module
	isReady: true,

	error: function( msg ) {
		throw new Error( msg );
	},

	noop: function() {},

	// See test/unit/core.js for details concerning isFunction.
	// Since version 1.3, DOM methods and functions like alert
	// aren't supported. They return false on IE (#2968).
	isFunction: function( obj ) {
		return jQuery.type( obj ) === "function";
	},

	isArray: Array.isArray || function( obj ) {
		return jQuery.type( obj ) === "array";
	},

	isWindow: function( obj ) {
		/* jshint eqeqeq: false */
		return obj != null && obj == obj.window;
	},

	isNumeric: function( obj ) {

		// parseFloat NaNs numeric-cast false positives (null|true|false|"")
		// ...but misinterprets leading-number strings, particularly hex literals ("0x...")
		// subtraction forces infinities to NaN
		// adding 1 corrects loss of precision from parseFloat (#15100)
		var realStringObj = obj && obj.toString();
		return !jQuery.isArray( obj ) && ( realStringObj - parseFloat( realStringObj ) + 1 ) >= 0;
	},

	isEmptyObject: function( obj ) {
		var name;
		for ( name in obj ) {
			return false;
		}
		return true;
	},

	isPlainObject: function( obj ) {
		var key;

		// Must be an Object.
		// Because of IE, we also have to check the presence of the constructor property.
		// Make sure that DOM nodes and window objects don't pass through, as well
		if ( !obj || jQuery.type( obj ) !== "object" || obj.nodeType || jQuery.isWindow( obj ) ) {
			return false;
		}

		try {

			// Not own constructor property must be Object
			if ( obj.constructor &&
				!hasOwn.call( obj, "constructor" ) &&
				!hasOwn.call( obj.constructor.prototype, "isPrototypeOf" ) ) {
				return false;
			}
		} catch ( e ) {

			// IE8,9 Will throw exceptions on certain host objects #9897
			return false;
		}

		// Support: IE<9
		// Handle iteration over inherited properties before own properties.
		if ( !support.ownFirst ) {
			for ( key in obj ) {
				return hasOwn.call( obj, key );
			}
		}

		// Own properties are enumerated firstly, so to speed up,
		// if last one is own, then all properties are own.
		for ( key in obj ) {}

		return key === undefined || hasOwn.call( obj, key );
	},

	type: function( obj ) {
		if ( obj == null ) {
			return obj + "";
		}
		return typeof obj === "object" || typeof obj === "function" ?
			class2type[ toString.call( obj ) ] || "object" :
			typeof obj;
	},

	// Workarounds based on findings by Jim Driscoll
	// http://weblogs.java.net/blog/driscoll/archive/2009/09/08/eval-javascript-global-context
	globalEval: function( data ) {
		if ( data && jQuery.trim( data ) ) {

			// We use execScript on Internet Explorer
			// We use an anonymous function so that context is window
			// rather than jQuery in Firefox
			( window.execScript || function( data ) {
				window[ "eval" ].call( window, data ); // jscs:ignore requireDotNotation
			} )( data );
		}
	},

	// Convert dashed to camelCase; used by the css and data modules
	// Microsoft forgot to hump their vendor prefix (#9572)
	camelCase: function( string ) {
		return string.replace( rmsPrefix, "ms-" ).replace( rdashAlpha, fcamelCase );
	},

	nodeName: function( elem, name ) {
		return elem.nodeName && elem.nodeName.toLowerCase() === name.toLowerCase();
	},

	each: function( obj, callback ) {
		var length, i = 0;

		if ( isArrayLike( obj ) ) {
			length = obj.length;
			for ( ; i < length; i++ ) {
				if ( callback.call( obj[ i ], i, obj[ i ] ) === false ) {
					break;
				}
			}
		} else {
			for ( i in obj ) {
				if ( callback.call( obj[ i ], i, obj[ i ] ) === false ) {
					break;
				}
			}
		}

		return obj;
	},

	// Support: Android<4.1, IE<9
	trim: function( text ) {
		return text == null ?
			"" :
			( text + "" ).replace( rtrim, "" );
	},

	// results is for internal usage only
	makeArray: function( arr, results ) {
		var ret = results || [];

		if ( arr != null ) {
			if ( isArrayLike( Object( arr ) ) ) {
				jQuery.merge( ret,
					typeof arr === "string" ?
					[ arr ] : arr
				);
			} else {
				push.call( ret, arr );
			}
		}

		return ret;
	},

	inArray: function( elem, arr, i ) {
		var len;

		if ( arr ) {
			if ( indexOf ) {
				return indexOf.call( arr, elem, i );
			}

			len = arr.length;
			i = i ? i < 0 ? Math.max( 0, len + i ) : i : 0;

			for ( ; i < len; i++ ) {

				// Skip accessing in sparse arrays
				if ( i in arr && arr[ i ] === elem ) {
					return i;
				}
			}
		}

		return -1;
	},

	merge: function( first, second ) {
		var len = +second.length,
			j = 0,
			i = first.length;

		while ( j < len ) {
			first[ i++ ] = second[ j++ ];
		}

		// Support: IE<9
		// Workaround casting of .length to NaN on otherwise arraylike objects (e.g., NodeLists)
		if ( len !== len ) {
			while ( second[ j ] !== undefined ) {
				first[ i++ ] = second[ j++ ];
			}
		}

		first.length = i;

		return first;
	},

	grep: function( elems, callback, invert ) {
		var callbackInverse,
			matches = [],
			i = 0,
			length = elems.length,
			callbackExpect = !invert;

		// Go through the array, only saving the items
		// that pass the validator function
		for ( ; i < length; i++ ) {
			callbackInverse = !callback( elems[ i ], i );
			if ( callbackInverse !== callbackExpect ) {
				matches.push( elems[ i ] );
			}
		}

		return matches;
	},

	// arg is for internal usage only
	map: function( elems, callback, arg ) {
		var length, value,
			i = 0,
			ret = [];

		// Go through the array, translating each of the items to their new values
		if ( isArrayLike( elems ) ) {
			length = elems.length;
			for ( ; i < length; i++ ) {
				value = callback( elems[ i ], i, arg );

				if ( value != null ) {
					ret.push( value );
				}
			}

		// Go through every key on the object,
		} else {
			for ( i in elems ) {
				value = callback( elems[ i ], i, arg );

				if ( value != null ) {
					ret.push( value );
				}
			}
		}

		// Flatten any nested arrays
		return concat.apply( [], ret );
	},

	// A global GUID counter for objects
	guid: 1,

	// Bind a function to a context, optionally partially applying any
	// arguments.
	proxy: function( fn, context ) {
		var args, proxy, tmp;

		if ( typeof context === "string" ) {
			tmp = fn[ context ];
			context = fn;
			fn = tmp;
		}

		// Quick check to determine if target is callable, in the spec
		// this throws a TypeError, but we will just return undefined.
		if ( !jQuery.isFunction( fn ) ) {
			return undefined;
		}

		// Simulated bind
		args = slice.call( arguments, 2 );
		proxy = function() {
			return fn.apply( context || this, args.concat( slice.call( arguments ) ) );
		};

		// Set the guid of unique handler to the same of original handler, so it can be removed
		proxy.guid = fn.guid = fn.guid || jQuery.guid++;

		return proxy;
	},

	now: function() {
		return +( new Date() );
	},

	// jQuery.support is not used in Core but other projects attach their
	// properties to it so it needs to exist.
	support: support
} );

// JSHint would error on this code due to the Symbol not being defined in ES5.
// Defining this global in .jshintrc would create a danger of using the global
// unguarded in another place, it seems safer to just disable JSHint for these
// three lines.
/* jshint ignore: start */
if ( typeof Symbol === "function" ) {
	jQuery.fn[ Symbol.iterator ] = deletedIds[ Symbol.iterator ];
}
/* jshint ignore: end */

// Populate the class2type map
jQuery.each( "Boolean Number String Function Array Date RegExp Object Error Symbol".split( " " ),
function( i, name ) {
	class2type[ "[object " + name + "]" ] = name.toLowerCase();
} );

function isArrayLike( obj ) {

	// Support: iOS 8.2 (not reproducible in simulator)
	// `in` check used to prevent JIT error (gh-2145)
	// hasOwn isn't used here due to false negatives
	// regarding Nodelist length in IE
	var length = !!obj && "length" in obj && obj.length,
		type = jQuery.type( obj );

	if ( type === "function" || jQuery.isWindow( obj ) ) {
		return false;
	}

	return type === "array" || length === 0 ||
		typeof length === "number" && length > 0 && ( length - 1 ) in obj;
}
var Sizzle =
/*!
 * Sizzle CSS Selector Engine v2.2.1
 * http://sizzlejs.com/
 *
 * Copyright jQuery Foundation and other contributors
 * Released under the MIT license
 * http://jquery.org/license
 *
 * Date: 2015-10-17
 */
(function( window ) {

var i,
	support,
	Expr,
	getText,
	isXML,
	tokenize,
	compile,
	select,
	outermostContext,
	sortInput,
	hasDuplicate,

	// Local document vars
	setDocument,
	document,
	docElem,
	documentIsHTML,
	rbuggyQSA,
	rbuggyMatches,
	matches,
	contains,

	// Instance-specific data
	expando = "sizzle" + 1 * new Date(),
	preferredDoc = window.document,
	dirruns = 0,
	done = 0,
	classCache = createCache(),
	tokenCache = createCache(),
	compilerCache = createCache(),
	sortOrder = function( a, b ) {
		if ( a === b ) {
			hasDuplicate = true;
		}
		return 0;
	},

	// General-purpose constants
	MAX_NEGATIVE = 1 << 31,

	// Instance methods
	hasOwn = ({}).hasOwnProperty,
	arr = [],
	pop = arr.pop,
	push_native = arr.push,
	push = arr.push,
	slice = arr.slice,
	// Use a stripped-down indexOf as it's faster than native
	// http://jsperf.com/thor-indexof-vs-for/5
	indexOf = function( list, elem ) {
		var i = 0,
			len = list.length;
		for ( ; i < len; i++ ) {
			if ( list[i] === elem ) {
				return i;
			}
		}
		return -1;
	},

	booleans = "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",

	// Regular expressions

	// http://www.w3.org/TR/css3-selectors/#whitespace
	whitespace = "[\\x20\\t\\r\\n\\f]",

	// http://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
	identifier = "(?:\\\\.|[\\w-]|[^\\x00-\\xa0])+",

	// Attribute selectors: http://www.w3.org/TR/selectors/#attribute-selectors
	attributes = "\\[" + whitespace + "*(" + identifier + ")(?:" + whitespace +
		// Operator (capture 2)
		"*([*^$|!~]?=)" + whitespace +
		// "Attribute values must be CSS identifiers [capture 5] or strings [capture 3 or capture 4]"
		"*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(" + identifier + "))|)" + whitespace +
		"*\\]",

	pseudos = ":(" + identifier + ")(?:\\((" +
		// To reduce the number of selectors needing tokenize in the preFilter, prefer arguments:
		// 1. quoted (capture 3; capture 4 or capture 5)
		"('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|" +
		// 2. simple (capture 6)
		"((?:\\\\.|[^\\\\()[\\]]|" + attributes + ")*)|" +
		// 3. anything else (capture 2)
		".*" +
		")\\)|)",

	// Leading and non-escaped trailing whitespace, capturing some non-whitespace characters preceding the latter
	rwhitespace = new RegExp( whitespace + "+", "g" ),
	rtrim = new RegExp( "^" + whitespace + "+|((?:^|[^\\\\])(?:\\\\.)*)" + whitespace + "+$", "g" ),

	rcomma = new RegExp( "^" + whitespace + "*," + whitespace + "*" ),
	rcombinators = new RegExp( "^" + whitespace + "*([>+~]|" + whitespace + ")" + whitespace + "*" ),

	rattributeQuotes = new RegExp( "=" + whitespace + "*([^\\]'\"]*?)" + whitespace + "*\\]", "g" ),

	rpseudo = new RegExp( pseudos ),
	ridentifier = new RegExp( "^" + identifier + "$" ),

	matchExpr = {
		"ID": new RegExp( "^#(" + identifier + ")" ),
		"CLASS": new RegExp( "^\\.(" + identifier + ")" ),
		"TAG": new RegExp( "^(" + identifier + "|[*])" ),
		"ATTR": new RegExp( "^" + attributes ),
		"PSEUDO": new RegExp( "^" + pseudos ),
		"CHILD": new RegExp( "^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\(" + whitespace +
			"*(even|odd|(([+-]|)(\\d*)n|)" + whitespace + "*(?:([+-]|)" + whitespace +
			"*(\\d+)|))" + whitespace + "*\\)|)", "i" ),
		"bool": new RegExp( "^(?:" + booleans + ")$", "i" ),
		// For use in libraries implementing .is()
		// We use this for POS matching in `select`
		"needsContext": new RegExp( "^" + whitespace + "*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\(" +
			whitespace + "*((?:-\\d)?\\d*)" + whitespace + "*\\)|)(?=[^-]|$)", "i" )
	},

	rinputs = /^(?:input|select|textarea|button)$/i,
	rheader = /^h\d$/i,

	rnative = /^[^{]+\{\s*\[native \w/,

	// Easily-parseable/retrievable ID or TAG or CLASS selectors
	rquickExpr = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,

	rsibling = /[+~]/,
	rescape = /'|\\/g,

	// CSS escapes http://www.w3.org/TR/CSS21/syndata.html#escaped-characters
	runescape = new RegExp( "\\\\([\\da-f]{1,6}" + whitespace + "?|(" + whitespace + ")|.)", "ig" ),
	funescape = function( _, escaped, escapedWhitespace ) {
		var high = "0x" + escaped - 0x10000;
		// NaN means non-codepoint
		// Support: Firefox<24
		// Workaround erroneous numeric interpretation of +"0x"
		return high !== high || escapedWhitespace ?
			escaped :
			high < 0 ?
				// BMP codepoint
				String.fromCharCode( high + 0x10000 ) :
				// Supplemental Plane codepoint (surrogate pair)
				String.fromCharCode( high >> 10 | 0xD800, high & 0x3FF | 0xDC00 );
	},

	// Used for iframes
	// See setDocument()
	// Removing the function wrapper causes a "Permission Denied"
	// error in IE
	unloadHandler = function() {
		setDocument();
	};

// Optimize for push.apply( _, NodeList )
try {
	push.apply(
		(arr = slice.call( preferredDoc.childNodes )),
		preferredDoc.childNodes
	);
	// Support: Android<4.0
	// Detect silently failing push.apply
	arr[ preferredDoc.childNodes.length ].nodeType;
} catch ( e ) {
	push = { apply: arr.length ?

		// Leverage slice if possible
		function( target, els ) {
			push_native.apply( target, slice.call(els) );
		} :

		// Support: IE<9
		// Otherwise append directly
		function( target, els ) {
			var j = target.length,
				i = 0;
			// Can't trust NodeList.length
			while ( (target[j++] = els[i++]) ) {}
			target.length = j - 1;
		}
	};
}

function Sizzle( selector, context, results, seed ) {
	var m, i, elem, nid, nidselect, match, groups, newSelector,
		newContext = context && context.ownerDocument,

		// nodeType defaults to 9, since context defaults to document
		nodeType = context ? context.nodeType : 9;

	results = results || [];

	// Return early from calls with invalid selector or context
	if ( typeof selector !== "string" || !selector ||
		nodeType !== 1 && nodeType !== 9 && nodeType !== 11 ) {

		return results;
	}

	// Try to shortcut find operations (as opposed to filters) in HTML documents
	if ( !seed ) {

		if ( ( context ? context.ownerDocument || context : preferredDoc ) !== document ) {
			setDocument( context );
		}
		context = context || document;

		if ( documentIsHTML ) {

			// If the selector is sufficiently simple, try using a "get*By*" DOM method
			// (excepting DocumentFragment context, where the methods don't exist)
			if ( nodeType !== 11 && (match = rquickExpr.exec( selector )) ) {

				// ID selector
				if ( (m = match[1]) ) {

					// Document context
					if ( nodeType === 9 ) {
						if ( (elem = context.getElementById( m )) ) {

							// Support: IE, Opera, Webkit
							// TODO: identify versions
							// getElementById can match elements by name instead of ID
							if ( elem.id === m ) {
								results.push( elem );
								return results;
							}
						} else {
							return results;
						}

					// Element context
					} else {

						// Support: IE, Opera, Webkit
						// TODO: identify versions
						// getElementById can match elements by name instead of ID
						if ( newContext && (elem = newContext.getElementById( m )) &&
							contains( context, elem ) &&
							elem.id === m ) {

							results.push( elem );
							return results;
						}
					}

				// Type selector
				} else if ( match[2] ) {
					push.apply( results, context.getElementsByTagName( selector ) );
					return results;

				// Class selector
				} else if ( (m = match[3]) && support.getElementsByClassName &&
					context.getElementsByClassName ) {

					push.apply( results, context.getElementsByClassName( m ) );
					return results;
				}
			}

			// Take advantage of querySelectorAll
			if ( support.qsa &&
				!compilerCache[ selector + " " ] &&
				(!rbuggyQSA || !rbuggyQSA.test( selector )) ) {

				if ( nodeType !== 1 ) {
					newContext = context;
					newSelector = selector;

				// qSA looks outside Element context, which is not what we want
				// Thanks to Andrew Dupont for this workaround technique
				// Support: IE <=8
				// Exclude object elements
				} else if ( context.nodeName.toLowerCase() !== "object" ) {

					// Capture the context ID, setting it first if necessary
					if ( (nid = context.getAttribute( "id" )) ) {
						nid = nid.replace( rescape, "\\$&" );
					} else {
						context.setAttribute( "id", (nid = expando) );
					}

					// Prefix every selector in the list
					groups = tokenize( selector );
					i = groups.length;
					nidselect = ridentifier.test( nid ) ? "#" + nid : "[id='" + nid + "']";
					while ( i-- ) {
						groups[i] = nidselect + " " + toSelector( groups[i] );
					}
					newSelector = groups.join( "," );

					// Expand context for sibling selectors
					newContext = rsibling.test( selector ) && testContext( context.parentNode ) ||
						context;
				}

				if ( newSelector ) {
					try {
						push.apply( results,
							newContext.querySelectorAll( newSelector )
						);
						return results;
					} catch ( qsaError ) {
					} finally {
						if ( nid === expando ) {
							context.removeAttribute( "id" );
						}
					}
				}
			}
		}
	}

	// All others
	return select( selector.replace( rtrim, "$1" ), context, results, seed );
}

/**
 * Create key-value caches of limited size
 * @returns {function(string, object)} Returns the Object data after storing it on itself with
 *	property name the (space-suffixed) string and (if the cache is larger than Expr.cacheLength)
 *	deleting the oldest entry
 */
function createCache() {
	var keys = [];

	function cache( key, value ) {
		// Use (key + " ") to avoid collision with native prototype properties (see Issue #157)
		if ( keys.push( key + " " ) > Expr.cacheLength ) {
			// Only keep the most recent entries
			delete cache[ keys.shift() ];
		}
		return (cache[ key + " " ] = value);
	}
	return cache;
}

/**
 * Mark a function for special use by Sizzle
 * @param {Function} fn The function to mark
 */
function markFunction( fn ) {
	fn[ expando ] = true;
	return fn;
}

/**
 * Support testing using an element
 * @param {Function} fn Passed the created div and expects a boolean result
 */
function assert( fn ) {
	var div = document.createElement("div");

	try {
		return !!fn( div );
	} catch (e) {
		return false;
	} finally {
		// Remove from its parent by default
		if ( div.parentNode ) {
			div.parentNode.removeChild( div );
		}
		// release memory in IE
		div = null;
	}
}

/**
 * Adds the same handler for all of the specified attrs
 * @param {String} attrs Pipe-separated list of attributes
 * @param {Function} handler The method that will be applied
 */
function addHandle( attrs, handler ) {
	var arr = attrs.split("|"),
		i = arr.length;

	while ( i-- ) {
		Expr.attrHandle[ arr[i] ] = handler;
	}
}

/**
 * Checks document order of two siblings
 * @param {Element} a
 * @param {Element} b
 * @returns {Number} Returns less than 0 if a precedes b, greater than 0 if a follows b
 */
function siblingCheck( a, b ) {
	var cur = b && a,
		diff = cur && a.nodeType === 1 && b.nodeType === 1 &&
			( ~b.sourceIndex || MAX_NEGATIVE ) -
			( ~a.sourceIndex || MAX_NEGATIVE );

	// Use IE sourceIndex if available on both nodes
	if ( diff ) {
		return diff;
	}

	// Check if b follows a
	if ( cur ) {
		while ( (cur = cur.nextSibling) ) {
			if ( cur === b ) {
				return -1;
			}
		}
	}

	return a ? 1 : -1;
}

/**
 * Returns a function to use in pseudos for input types
 * @param {String} type
 */
function createInputPseudo( type ) {
	return function( elem ) {
		var name = elem.nodeName.toLowerCase();
		return name === "input" && elem.type === type;
	};
}

/**
 * Returns a function to use in pseudos for buttons
 * @param {String} type
 */
function createButtonPseudo( type ) {
	return function( elem ) {
		var name = elem.nodeName.toLowerCase();
		return (name === "input" || name === "button") && elem.type === type;
	};
}

/**
 * Returns a function to use in pseudos for positionals
 * @param {Function} fn
 */
function createPositionalPseudo( fn ) {
	return markFunction(function( argument ) {
		argument = +argument;
		return markFunction(function( seed, matches ) {
			var j,
				matchIndexes = fn( [], seed.length, argument ),
				i = matchIndexes.length;

			// Match elements found at the specified indexes
			while ( i-- ) {
				if ( seed[ (j = matchIndexes[i]) ] ) {
					seed[j] = !(matches[j] = seed[j]);
				}
			}
		});
	});
}

/**
 * Checks a node for validity as a Sizzle context
 * @param {Element|Object=} context
 * @returns {Element|Object|Boolean} The input node if acceptable, otherwise a falsy value
 */
function testContext( context ) {
	return context && typeof context.getElementsByTagName !== "undefined" && context;
}

// Expose support vars for convenience
support = Sizzle.support = {};

/**
 * Detects XML nodes
 * @param {Element|Object} elem An element or a document
 * @returns {Boolean} True iff elem is a non-HTML XML node
 */
isXML = Sizzle.isXML = function( elem ) {
	// documentElement is verified for cases where it doesn't yet exist
	// (such as loading iframes in IE - #4833)
	var documentElement = elem && (elem.ownerDocument || elem).documentElement;
	return documentElement ? documentElement.nodeName !== "HTML" : false;
};

/**
 * Sets document-related variables once based on the current document
 * @param {Element|Object} [doc] An element or document object to use to set the document
 * @returns {Object} Returns the current document
 */
setDocument = Sizzle.setDocument = function( node ) {
	var hasCompare, parent,
		doc = node ? node.ownerDocument || node : preferredDoc;

	// Return early if doc is invalid or already selected
	if ( doc === document || doc.nodeType !== 9 || !doc.documentElement ) {
		return document;
	}

	// Update global variables
	document = doc;
	docElem = document.documentElement;
	documentIsHTML = !isXML( document );

	// Support: IE 9-11, Edge
	// Accessing iframe documents after unload throws "permission denied" errors (jQuery #13936)
	if ( (parent = document.defaultView) && parent.top !== parent ) {
		// Support: IE 11
		if ( parent.addEventListener ) {
			parent.addEventListener( "unload", unloadHandler, false );

		// Support: IE 9 - 10 only
		} else if ( parent.attachEvent ) {
			parent.attachEvent( "onunload", unloadHandler );
		}
	}

	/* Attributes
	---------------------------------------------------------------------- */

	// Support: IE<8
	// Verify that getAttribute really returns attributes and not properties
	// (excepting IE8 booleans)
	support.attributes = assert(function( div ) {
		div.className = "i";
		return !div.getAttribute("className");
	});

	/* getElement(s)By*
	---------------------------------------------------------------------- */

	// Check if getElementsByTagName("*") returns only elements
	support.getElementsByTagName = assert(function( div ) {
		div.appendChild( document.createComment("") );
		return !div.getElementsByTagName("*").length;
	});

	// Support: IE<9
	support.getElementsByClassName = rnative.test( document.getElementsByClassName );

	// Support: IE<10
	// Check if getElementById returns elements by name
	// The broken getElementById methods don't pick up programatically-set names,
	// so use a roundabout getElementsByName test
	support.getById = assert(function( div ) {
		docElem.appendChild( div ).id = expando;
		return !document.getElementsByName || !document.getElementsByName( expando ).length;
	});

	// ID find and filter
	if ( support.getById ) {
		Expr.find["ID"] = function( id, context ) {
			if ( typeof context.getElementById !== "undefined" && documentIsHTML ) {
				var m = context.getElementById( id );
				return m ? [ m ] : [];
			}
		};
		Expr.filter["ID"] = function( id ) {
			var attrId = id.replace( runescape, funescape );
			return function( elem ) {
				return elem.getAttribute("id") === attrId;
			};
		};
	} else {
		// Support: IE6/7
		// getElementById is not reliable as a find shortcut
		delete Expr.find["ID"];

		Expr.filter["ID"] =  function( id ) {
			var attrId = id.replace( runescape, funescape );
			return function( elem ) {
				var node = typeof elem.getAttributeNode !== "undefined" &&
					elem.getAttributeNode("id");
				return node && node.value === attrId;
			};
		};
	}

	// Tag
	Expr.find["TAG"] = support.getElementsByTagName ?
		function( tag, context ) {
			if ( typeof context.getElementsByTagName !== "undefined" ) {
				return context.getElementsByTagName( tag );

			// DocumentFragment nodes don't have gEBTN
			} else if ( support.qsa ) {
				return context.querySelectorAll( tag );
			}
		} :

		function( tag, context ) {
			var elem,
				tmp = [],
				i = 0,
				// By happy coincidence, a (broken) gEBTN appears on DocumentFragment nodes too
				results = context.getElementsByTagName( tag );

			// Filter out possible comments
			if ( tag === "*" ) {
				while ( (elem = results[i++]) ) {
					if ( elem.nodeType === 1 ) {
						tmp.push( elem );
					}
				}

				return tmp;
			}
			return results;
		};

	// Class
	Expr.find["CLASS"] = support.getElementsByClassName && function( className, context ) {
		if ( typeof context.getElementsByClassName !== "undefined" && documentIsHTML ) {
			return context.getElementsByClassName( className );
		}
	};

	/* QSA/matchesSelector
	---------------------------------------------------------------------- */

	// QSA and matchesSelector support

	// matchesSelector(:active) reports false when true (IE9/Opera 11.5)
	rbuggyMatches = [];

	// qSa(:focus) reports false when true (Chrome 21)
	// We allow this because of a bug in IE8/9 that throws an error
	// whenever `document.activeElement` is accessed on an iframe
	// So, we allow :focus to pass through QSA all the time to avoid the IE error
	// See http://bugs.jquery.com/ticket/13378
	rbuggyQSA = [];

	if ( (support.qsa = rnative.test( document.querySelectorAll )) ) {
		// Build QSA regex
		// Regex strategy adopted from Diego Perini
		assert(function( div ) {
			// Select is set to empty string on purpose
			// This is to test IE's treatment of not explicitly
			// setting a boolean content attribute,
			// since its presence should be enough
			// http://bugs.jquery.com/ticket/12359
			docElem.appendChild( div ).innerHTML = "<a id='" + expando + "'></a>" +
				"<select id='" + expando + "-\r\\' msallowcapture=''>" +
				"<option selected=''></option></select>";

			// Support: IE8, Opera 11-12.16
			// Nothing should be selected when empty strings follow ^= or $= or *=
			// The test attribute must be unknown in Opera but "safe" for WinRT
			// http://msdn.microsoft.com/en-us/library/ie/hh465388.aspx#attribute_section
			if ( div.querySelectorAll("[msallowcapture^='']").length ) {
				rbuggyQSA.push( "[*^$]=" + whitespace + "*(?:''|\"\")" );
			}

			// Support: IE8
			// Boolean attributes and "value" are not treated correctly
			if ( !div.querySelectorAll("[selected]").length ) {
				rbuggyQSA.push( "\\[" + whitespace + "*(?:value|" + booleans + ")" );
			}

			// Support: Chrome<29, Android<4.4, Safari<7.0+, iOS<7.0+, PhantomJS<1.9.8+
			if ( !div.querySelectorAll( "[id~=" + expando + "-]" ).length ) {
				rbuggyQSA.push("~=");
			}

			// Webkit/Opera - :checked should return selected option elements
			// http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#checked
			// IE8 throws error here and will not see later tests
			if ( !div.querySelectorAll(":checked").length ) {
				rbuggyQSA.push(":checked");
			}

			// Support: Safari 8+, iOS 8+
			// https://bugs.webkit.org/show_bug.cgi?id=136851
			// In-page `selector#id sibing-combinator selector` fails
			if ( !div.querySelectorAll( "a#" + expando + "+*" ).length ) {
				rbuggyQSA.push(".#.+[+~]");
			}
		});

		assert(function( div ) {
			// Support: Windows 8 Native Apps
			// The type and name attributes are restricted during .innerHTML assignment
			var input = document.createElement("input");
			input.setAttribute( "type", "hidden" );
			div.appendChild( input ).setAttribute( "name", "D" );

			// Support: IE8
			// Enforce case-sensitivity of name attribute
			if ( div.querySelectorAll("[name=d]").length ) {
				rbuggyQSA.push( "name" + whitespace + "*[*^$|!~]?=" );
			}

			// FF 3.5 - :enabled/:disabled and hidden elements (hidden elements are still enabled)
			// IE8 throws error here and will not see later tests
			if ( !div.querySelectorAll(":enabled").length ) {
				rbuggyQSA.push( ":enabled", ":disabled" );
			}

			// Opera 10-11 does not throw on post-comma invalid pseudos
			div.querySelectorAll("*,:x");
			rbuggyQSA.push(",.*:");
		});
	}

	if ( (support.matchesSelector = rnative.test( (matches = docElem.matches ||
		docElem.webkitMatchesSelector ||
		docElem.mozMatchesSelector ||
		docElem.oMatchesSelector ||
		docElem.msMatchesSelector) )) ) {

		assert(function( div ) {
			// Check to see if it's possible to do matchesSelector
			// on a disconnected node (IE 9)
			support.disconnectedMatch = matches.call( div, "div" );

			// This should fail with an exception
			// Gecko does not error, returns false instead
			matches.call( div, "[s!='']:x" );
			rbuggyMatches.push( "!=", pseudos );
		});
	}

	rbuggyQSA = rbuggyQSA.length && new RegExp( rbuggyQSA.join("|") );
	rbuggyMatches = rbuggyMatches.length && new RegExp( rbuggyMatches.join("|") );

	/* Contains
	---------------------------------------------------------------------- */
	hasCompare = rnative.test( docElem.compareDocumentPosition );

	// Element contains another
	// Purposefully self-exclusive
	// As in, an element does not contain itself
	contains = hasCompare || rnative.test( docElem.contains ) ?
		function( a, b ) {
			var adown = a.nodeType === 9 ? a.documentElement : a,
				bup = b && b.parentNode;
			return a === bup || !!( bup && bup.nodeType === 1 && (
				adown.contains ?
					adown.contains( bup ) :
					a.compareDocumentPosition && a.compareDocumentPosition( bup ) & 16
			));
		} :
		function( a, b ) {
			if ( b ) {
				while ( (b = b.parentNode) ) {
					if ( b === a ) {
						return true;
					}
				}
			}
			return false;
		};

	/* Sorting
	---------------------------------------------------------------------- */

	// Document order sorting
	sortOrder = hasCompare ?
	function( a, b ) {

		// Flag for duplicate removal
		if ( a === b ) {
			hasDuplicate = true;
			return 0;
		}

		// Sort on method existence if only one input has compareDocumentPosition
		var compare = !a.compareDocumentPosition - !b.compareDocumentPosition;
		if ( compare ) {
			return compare;
		}

		// Calculate position if both inputs belong to the same document
		compare = ( a.ownerDocument || a ) === ( b.ownerDocument || b ) ?
			a.compareDocumentPosition( b ) :

			// Otherwise we know they are disconnected
			1;

		// Disconnected nodes
		if ( compare & 1 ||
			(!support.sortDetached && b.compareDocumentPosition( a ) === compare) ) {

			// Choose the first element that is related to our preferred document
			if ( a === document || a.ownerDocument === preferredDoc && contains(preferredDoc, a) ) {
				return -1;
			}
			if ( b === document || b.ownerDocument === preferredDoc && contains(preferredDoc, b) ) {
				return 1;
			}

			// Maintain original order
			return sortInput ?
				( indexOf( sortInput, a ) - indexOf( sortInput, b ) ) :
				0;
		}

		return compare & 4 ? -1 : 1;
	} :
	function( a, b ) {
		// Exit early if the nodes are identical
		if ( a === b ) {
			hasDuplicate = true;
			return 0;
		}

		var cur,
			i = 0,
			aup = a.parentNode,
			bup = b.parentNode,
			ap = [ a ],
			bp = [ b ];

		// Parentless nodes are either documents or disconnected
		if ( !aup || !bup ) {
			return a === document ? -1 :
				b === document ? 1 :
				aup ? -1 :
				bup ? 1 :
				sortInput ?
				( indexOf( sortInput, a ) - indexOf( sortInput, b ) ) :
				0;

		// If the nodes are siblings, we can do a quick check
		} else if ( aup === bup ) {
			return siblingCheck( a, b );
		}

		// Otherwise we need full lists of their ancestors for comparison
		cur = a;
		while ( (cur = cur.parentNode) ) {
			ap.unshift( cur );
		}
		cur = b;
		while ( (cur = cur.parentNode) ) {
			bp.unshift( cur );
		}

		// Walk down the tree looking for a discrepancy
		while ( ap[i] === bp[i] ) {
			i++;
		}

		return i ?
			// Do a sibling check if the nodes have a common ancestor
			siblingCheck( ap[i], bp[i] ) :

			// Otherwise nodes in our document sort first
			ap[i] === preferredDoc ? -1 :
			bp[i] === preferredDoc ? 1 :
			0;
	};

	return document;
};

Sizzle.matches = function( expr, elements ) {
	return Sizzle( expr, null, null, elements );
};

Sizzle.matchesSelector = function( elem, expr ) {
	// Set document vars if needed
	if ( ( elem.ownerDocument || elem ) !== document ) {
		setDocument( elem );
	}

	// Make sure that attribute selectors are quoted
	expr = expr.replace( rattributeQuotes, "='$1']" );

	if ( support.matchesSelector && documentIsHTML &&
		!compilerCache[ expr + " " ] &&
		( !rbuggyMatches || !rbuggyMatches.test( expr ) ) &&
		( !rbuggyQSA     || !rbuggyQSA.test( expr ) ) ) {

		try {
			var ret = matches.call( elem, expr );

			// IE 9's matchesSelector returns false on disconnected nodes
			if ( ret || support.disconnectedMatch ||
					// As well, disconnected nodes are said to be in a document
					// fragment in IE 9
					elem.document && elem.document.nodeType !== 11 ) {
				return ret;
			}
		} catch (e) {}
	}

	return Sizzle( expr, document, null, [ elem ] ).length > 0;
};

Sizzle.contains = function( context, elem ) {
	// Set document vars if needed
	if ( ( context.ownerDocument || context ) !== document ) {
		setDocument( context );
	}
	return contains( context, elem );
};

Sizzle.attr = function( elem, name ) {
	// Set document vars if needed
	if ( ( elem.ownerDocument || elem ) !== document ) {
		setDocument( elem );
	}

	var fn = Expr.attrHandle[ name.toLowerCase() ],
		// Don't get fooled by Object.prototype properties (jQuery #13807)
		val = fn && hasOwn.call( Expr.attrHandle, name.toLowerCase() ) ?
			fn( elem, name, !documentIsHTML ) :
			undefined;

	return val !== undefined ?
		val :
		support.attributes || !documentIsHTML ?
			elem.getAttribute( name ) :
			(val = elem.getAttributeNode(name)) && val.specified ?
				val.value :
				null;
};

Sizzle.error = function( msg ) {
	throw new Error( "Syntax error, unrecognized expression: " + msg );
};

/**
 * Document sorting and removing duplicates
 * @param {ArrayLike} results
 */
Sizzle.uniqueSort = function( results ) {
	var elem,
		duplicates = [],
		j = 0,
		i = 0;

	// Unless we *know* we can detect duplicates, assume their presence
	hasDuplicate = !support.detectDuplicates;
	sortInput = !support.sortStable && results.slice( 0 );
	results.sort( sortOrder );

	if ( hasDuplicate ) {
		while ( (elem = results[i++]) ) {
			if ( elem === results[ i ] ) {
				j = duplicates.push( i );
			}
		}
		while ( j-- ) {
			results.splice( duplicates[ j ], 1 );
		}
	}

	// Clear input after sorting to release objects
	// See https://github.com/jquery/sizzle/pull/225
	sortInput = null;

	return results;
};

/**
 * Utility function for retrieving the text value of an array of DOM nodes
 * @param {Array|Element} elem
 */
getText = Sizzle.getText = function( elem ) {
	var node,
		ret = "",
		i = 0,
		nodeType = elem.nodeType;

	if ( !nodeType ) {
		// If no nodeType, this is expected to be an array
		while ( (node = elem[i++]) ) {
			// Do not traverse comment nodes
			ret += getText( node );
		}
	} else if ( nodeType === 1 || nodeType === 9 || nodeType === 11 ) {
		// Use textContent for elements
		// innerText usage removed for consistency of new lines (jQuery #11153)
		if ( typeof elem.textContent === "string" ) {
			return elem.textContent;
		} else {
			// Traverse its children
			for ( elem = elem.firstChild; elem; elem = elem.nextSibling ) {
				ret += getText( elem );
			}
		}
	} else if ( nodeType === 3 || nodeType === 4 ) {
		return elem.nodeValue;
	}
	// Do not include comment or processing instruction nodes

	return ret;
};

Expr = Sizzle.selectors = {

	// Can be adjusted by the user
	cacheLength: 50,

	createPseudo: markFunction,

	match: matchExpr,

	attrHandle: {},

	find: {},

	relative: {
		">": { dir: "parentNode", first: true },
		" ": { dir: "parentNode" },
		"+": { dir: "previousSibling", first: true },
		"~": { dir: "previousSibling" }
	},

	preFilter: {
		"ATTR": function( match ) {
			match[1] = match[1].replace( runescape, funescape );

			// Move the given value to match[3] whether quoted or unquoted
			match[3] = ( match[3] || match[4] || match[5] || "" ).replace( runescape, funescape );

			if ( match[2] === "~=" ) {
				match[3] = " " + match[3] + " ";
			}

			return match.slice( 0, 4 );
		},

		"CHILD": function( match ) {
			/* matches from matchExpr["CHILD"]
				1 type (only|nth|...)
				2 what (child|of-type)
				3 argument (even|odd|\d*|\d*n([+-]\d+)?|...)
				4 xn-component of xn+y argument ([+-]?\d*n|)
				5 sign of xn-component
				6 x of xn-component
				7 sign of y-component
				8 y of y-component
			*/
			match[1] = match[1].toLowerCase();

			if ( match[1].slice( 0, 3 ) === "nth" ) {
				// nth-* requires argument
				if ( !match[3] ) {
					Sizzle.error( match[0] );
				}

				// numeric x and y parameters for Expr.filter.CHILD
				// remember that false/true cast respectively to 0/1
				match[4] = +( match[4] ? match[5] + (match[6] || 1) : 2 * ( match[3] === "even" || match[3] === "odd" ) );
				match[5] = +( ( match[7] + match[8] ) || match[3] === "odd" );

			// other types prohibit arguments
			} else if ( match[3] ) {
				Sizzle.error( match[0] );
			}

			return match;
		},

		"PSEUDO": function( match ) {
			var excess,
				unquoted = !match[6] && match[2];

			if ( matchExpr["CHILD"].test( match[0] ) ) {
				return null;
			}

			// Accept quoted arguments as-is
			if ( match[3] ) {
				match[2] = match[4] || match[5] || "";

			// Strip excess characters from unquoted arguments
			} else if ( unquoted && rpseudo.test( unquoted ) &&
				// Get excess from tokenize (recursively)
				(excess = tokenize( unquoted, true )) &&
				// advance to the next closing parenthesis
				(excess = unquoted.indexOf( ")", unquoted.length - excess ) - unquoted.length) ) {

				// excess is a negative index
				match[0] = match[0].slice( 0, excess );
				match[2] = unquoted.slice( 0, excess );
			}

			// Return only captures needed by the pseudo filter method (type and argument)
			return match.slice( 0, 3 );
		}
	},

	filter: {

		"TAG": function( nodeNameSelector ) {
			var nodeName = nodeNameSelector.replace( runescape, funescape ).toLowerCase();
			return nodeNameSelector === "*" ?
				function() { return true; } :
				function( elem ) {
					return elem.nodeName && elem.nodeName.toLowerCase() === nodeName;
				};
		},

		"CLASS": function( className ) {
			var pattern = classCache[ className + " " ];

			return pattern ||
				(pattern = new RegExp( "(^|" + whitespace + ")" + className + "(" + whitespace + "|$)" )) &&
				classCache( className, function( elem ) {
					return pattern.test( typeof elem.className === "string" && elem.className || typeof elem.getAttribute !== "undefined" && elem.getAttribute("class") || "" );
				});
		},

		"ATTR": function( name, operator, check ) {
			return function( elem ) {
				var result = Sizzle.attr( elem, name );

				if ( result == null ) {
					return operator === "!=";
				}
				if ( !operator ) {
					return true;
				}

				result += "";

				return operator === "=" ? result === check :
					operator === "!=" ? result !== check :
					operator === "^=" ? check && result.indexOf( check ) === 0 :
					operator === "*=" ? check && result.indexOf( check ) > -1 :
					operator === "$=" ? check && result.slice( -check.length ) === check :
					operator === "~=" ? ( " " + result.replace( rwhitespace, " " ) + " " ).indexOf( check ) > -1 :
					operator === "|=" ? result === check || result.slice( 0, check.length + 1 ) === check + "-" :
					false;
			};
		},

		"CHILD": function( type, what, argument, first, last ) {
			var simple = type.slice( 0, 3 ) !== "nth",
				forward = type.slice( -4 ) !== "last",
				ofType = what === "of-type";

			return first === 1 && last === 0 ?

				// Shortcut for :nth-*(n)
				function( elem ) {
					return !!elem.parentNode;
				} :

				function( elem, context, xml ) {
					var cache, uniqueCache, outerCache, node, nodeIndex, start,
						dir = simple !== forward ? "nextSibling" : "previousSibling",
						parent = elem.parentNode,
						name = ofType && elem.nodeName.toLowerCase(),
						useCache = !xml && !ofType,
						diff = false;

					if ( parent ) {

						// :(first|last|only)-(child|of-type)
						if ( simple ) {
							while ( dir ) {
								node = elem;
								while ( (node = node[ dir ]) ) {
									if ( ofType ?
										node.nodeName.toLowerCase() === name :
										node.nodeType === 1 ) {

										return false;
									}
								}
								// Reverse direction for :only-* (if we haven't yet done so)
								start = dir = type === "only" && !start && "nextSibling";
							}
							return true;
						}

						start = [ forward ? parent.firstChild : parent.lastChild ];

						// non-xml :nth-child(...) stores cache data on `parent`
						if ( forward && useCache ) {

							// Seek `elem` from a previously-cached index

							// ...in a gzip-friendly way
							node = parent;
							outerCache = node[ expando ] || (node[ expando ] = {});

							// Support: IE <9 only
							// Defend against cloned attroperties (jQuery gh-1709)
							uniqueCache = outerCache[ node.uniqueID ] ||
								(outerCache[ node.uniqueID ] = {});

							cache = uniqueCache[ type ] || [];
							nodeIndex = cache[ 0 ] === dirruns && cache[ 1 ];
							diff = nodeIndex && cache[ 2 ];
							node = nodeIndex && parent.childNodes[ nodeIndex ];

							while ( (node = ++nodeIndex && node && node[ dir ] ||

								// Fallback to seeking `elem` from the start
								(diff = nodeIndex = 0) || start.pop()) ) {

								// When found, cache indexes on `parent` and break
								if ( node.nodeType === 1 && ++diff && node === elem ) {
									uniqueCache[ type ] = [ dirruns, nodeIndex, diff ];
									break;
								}
							}

						} else {
							// Use previously-cached element index if available
							if ( useCache ) {
								// ...in a gzip-friendly way
								node = elem;
								outerCache = node[ expando ] || (node[ expando ] = {});

								// Support: IE <9 only
								// Defend against cloned attroperties (jQuery gh-1709)
								uniqueCache = outerCache[ node.uniqueID ] ||
									(outerCache[ node.uniqueID ] = {});

								cache = uniqueCache[ type ] || [];
								nodeIndex = cache[ 0 ] === dirruns && cache[ 1 ];
								diff = nodeIndex;
							}

							// xml :nth-child(...)
							// or :nth-last-child(...) or :nth(-last)?-of-type(...)
							if ( diff === false ) {
								// Use the same loop as above to seek `elem` from the start
								while ( (node = ++nodeIndex && node && node[ dir ] ||
									(diff = nodeIndex = 0) || start.pop()) ) {

									if ( ( ofType ?
										node.nodeName.toLowerCase() === name :
										node.nodeType === 1 ) &&
										++diff ) {

										// Cache the index of each encountered element
										if ( useCache ) {
											outerCache = node[ expando ] || (node[ expando ] = {});

											// Support: IE <9 only
											// Defend against cloned attroperties (jQuery gh-1709)
											uniqueCache = outerCache[ node.uniqueID ] ||
												(outerCache[ node.uniqueID ] = {});

											uniqueCache[ type ] = [ dirruns, diff ];
										}

										if ( node === elem ) {
											break;
										}
									}
								}
							}
						}

						// Incorporate the offset, then check against cycle size
						diff -= last;
						return diff === first || ( diff % first === 0 && diff / first >= 0 );
					}
				};
		},

		"PSEUDO": function( pseudo, argument ) {
			// pseudo-class names are case-insensitive
			// http://www.w3.org/TR/selectors/#pseudo-classes
			// Prioritize by case sensitivity in case custom pseudos are added with uppercase letters
			// Remember that setFilters inherits from pseudos
			var args,
				fn = Expr.pseudos[ pseudo ] || Expr.setFilters[ pseudo.toLowerCase() ] ||
					Sizzle.error( "unsupported pseudo: " + pseudo );

			// The user may use createPseudo to indicate that
			// arguments are needed to create the filter function
			// just as Sizzle does
			if ( fn[ expando ] ) {
				return fn( argument );
			}

			// But maintain support for old signatures
			if ( fn.length > 1 ) {
				args = [ pseudo, pseudo, "", argument ];
				return Expr.setFilters.hasOwnProperty( pseudo.toLowerCase() ) ?
					markFunction(function( seed, matches ) {
						var idx,
							matched = fn( seed, argument ),
							i = matched.length;
						while ( i-- ) {
							idx = indexOf( seed, matched[i] );
							seed[ idx ] = !( matches[ idx ] = matched[i] );
						}
					}) :
					function( elem ) {
						return fn( elem, 0, args );
					};
			}

			return fn;
		}
	},

	pseudos: {
		// Potentially complex pseudos
		"not": markFunction(function( selector ) {
			// Trim the selector passed to compile
			// to avoid treating leading and trailing
			// spaces as combinators
			var input = [],
				results = [],
				matcher = compile( selector.replace( rtrim, "$1" ) );

			return matcher[ expando ] ?
				markFunction(function( seed, matches, context, xml ) {
					var elem,
						unmatched = matcher( seed, null, xml, [] ),
						i = seed.length;

					// Match elements unmatched by `matcher`
					while ( i-- ) {
						if ( (elem = unmatched[i]) ) {
							seed[i] = !(matches[i] = elem);
						}
					}
				}) :
				function( elem, context, xml ) {
					input[0] = elem;
					matcher( input, null, xml, results );
					// Don't keep the element (issue #299)
					input[0] = null;
					return !results.pop();
				};
		}),

		"has": markFunction(function( selector ) {
			return function( elem ) {
				return Sizzle( selector, elem ).length > 0;
			};
		}),

		"contains": markFunction(function( text ) {
			text = text.replace( runescape, funescape );
			return function( elem ) {
				return ( elem.textContent || elem.innerText || getText( elem ) ).indexOf( text ) > -1;
			};
		}),

		// "Whether an element is represented by a :lang() selector
		// is based solely on the element's language value
		// being equal to the identifier C,
		// or beginning with the identifier C immediately followed by "-".
		// The matching of C against the element's language value is performed case-insensitively.
		// The identifier C does not have to be a valid language name."
		// http://www.w3.org/TR/selectors/#lang-pseudo
		"lang": markFunction( function( lang ) {
			// lang value must be a valid identifier
			if ( !ridentifier.test(lang || "") ) {
				Sizzle.error( "unsupported lang: " + lang );
			}
			lang = lang.replace( runescape, funescape ).toLowerCase();
			return function( elem ) {
				var elemLang;
				do {
					if ( (elemLang = documentIsHTML ?
						elem.lang :
						elem.getAttribute("xml:lang") || elem.getAttribute("lang")) ) {

						elemLang = elemLang.toLowerCase();
						return elemLang === lang || elemLang.indexOf( lang + "-" ) === 0;
					}
				} while ( (elem = elem.parentNode) && elem.nodeType === 1 );
				return false;
			};
		}),

		// Miscellaneous
		"target": function( elem ) {
			var hash = window.location && window.location.hash;
			return hash && hash.slice( 1 ) === elem.id;
		},

		"root": function( elem ) {
			return elem === docElem;
		},

		"focus": function( elem ) {
			return elem === document.activeElement && (!document.hasFocus || document.hasFocus()) && !!(elem.type || elem.href || ~elem.tabIndex);
		},

		// Boolean properties
		"enabled": function( elem ) {
			return elem.disabled === false;
		},

		"disabled": function( elem ) {
			return elem.disabled === true;
		},

		"checked": function( elem ) {
			// In CSS3, :checked should return both checked and selected elements
			// http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#checked
			var nodeName = elem.nodeName.toLowerCase();
			return (nodeName === "input" && !!elem.checked) || (nodeName === "option" && !!elem.selected);
		},

		"selected": function( elem ) {
			// Accessing this property makes selected-by-default
			// options in Safari work properly
			if ( elem.parentNode ) {
				elem.parentNode.selectedIndex;
			}

			return elem.selected === true;
		},

		// Contents
		"empty": function( elem ) {
			// http://www.w3.org/TR/selectors/#empty-pseudo
			// :empty is negated by element (1) or content nodes (text: 3; cdata: 4; entity ref: 5),
			//   but not by others (comment: 8; processing instruction: 7; etc.)
			// nodeType < 6 works because attributes (2) do not appear as children
			for ( elem = elem.firstChild; elem; elem = elem.nextSibling ) {
				if ( elem.nodeType < 6 ) {
					return false;
				}
			}
			return true;
		},

		"parent": function( elem ) {
			return !Expr.pseudos["empty"]( elem );
		},

		// Element/input types
		"header": function( elem ) {
			return rheader.test( elem.nodeName );
		},

		"input": function( elem ) {
			return rinputs.test( elem.nodeName );
		},

		"button": function( elem ) {
			var name = elem.nodeName.toLowerCase();
			return name === "input" && elem.type === "button" || name === "button";
		},

		"text": function( elem ) {
			var attr;
			return elem.nodeName.toLowerCase() === "input" &&
				elem.type === "text" &&

				// Support: IE<8
				// New HTML5 attribute values (e.g., "search") appear with elem.type === "text"
				( (attr = elem.getAttribute("type")) == null || attr.toLowerCase() === "text" );
		},

		// Position-in-collection
		"first": createPositionalPseudo(function() {
			return [ 0 ];
		}),

		"last": createPositionalPseudo(function( matchIndexes, length ) {
			return [ length - 1 ];
		}),

		"eq": createPositionalPseudo(function( matchIndexes, length, argument ) {
			return [ argument < 0 ? argument + length : argument ];
		}),

		"even": createPositionalPseudo(function( matchIndexes, length ) {
			var i = 0;
			for ( ; i < length; i += 2 ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		}),

		"odd": createPositionalPseudo(function( matchIndexes, length ) {
			var i = 1;
			for ( ; i < length; i += 2 ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		}),

		"lt": createPositionalPseudo(function( matchIndexes, length, argument ) {
			var i = argument < 0 ? argument + length : argument;
			for ( ; --i >= 0; ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		}),

		"gt": createPositionalPseudo(function( matchIndexes, length, argument ) {
			var i = argument < 0 ? argument + length : argument;
			for ( ; ++i < length; ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		})
	}
};

Expr.pseudos["nth"] = Expr.pseudos["eq"];

// Add button/input type pseudos
for ( i in { radio: true, checkbox: true, file: true, password: true, image: true } ) {
	Expr.pseudos[ i ] = createInputPseudo( i );
}
for ( i in { submit: true, reset: true } ) {
	Expr.pseudos[ i ] = createButtonPseudo( i );
}

// Easy API for creating new setFilters
function setFilters() {}
setFilters.prototype = Expr.filters = Expr.pseudos;
Expr.setFilters = new setFilters();

tokenize = Sizzle.tokenize = function( selector, parseOnly ) {
	var matched, match, tokens, type,
		soFar, groups, preFilters,
		cached = tokenCache[ selector + " " ];

	if ( cached ) {
		return parseOnly ? 0 : cached.slice( 0 );
	}

	soFar = selector;
	groups = [];
	preFilters = Expr.preFilter;

	while ( soFar ) {

		// Comma and first run
		if ( !matched || (match = rcomma.exec( soFar )) ) {
			if ( match ) {
				// Don't consume trailing commas as valid
				soFar = soFar.slice( match[0].length ) || soFar;
			}
			groups.push( (tokens = []) );
		}

		matched = false;

		// Combinators
		if ( (match = rcombinators.exec( soFar )) ) {
			matched = match.shift();
			tokens.push({
				value: matched,
				// Cast descendant combinators to space
				type: match[0].replace( rtrim, " " )
			});
			soFar = soFar.slice( matched.length );
		}

		// Filters
		for ( type in Expr.filter ) {
			if ( (match = matchExpr[ type ].exec( soFar )) && (!preFilters[ type ] ||
				(match = preFilters[ type ]( match ))) ) {
				matched = match.shift();
				tokens.push({
					value: matched,
					type: type,
					matches: match
				});
				soFar = soFar.slice( matched.length );
			}
		}

		if ( !matched ) {
			break;
		}
	}

	// Return the length of the invalid excess
	// if we're just parsing
	// Otherwise, throw an error or return tokens
	return parseOnly ?
		soFar.length :
		soFar ?
			Sizzle.error( selector ) :
			// Cache the tokens
			tokenCache( selector, groups ).slice( 0 );
};

function toSelector( tokens ) {
	var i = 0,
		len = tokens.length,
		selector = "";
	for ( ; i < len; i++ ) {
		selector += tokens[i].value;
	}
	return selector;
}

function addCombinator( matcher, combinator, base ) {
	var dir = combinator.dir,
		checkNonElements = base && dir === "parentNode",
		doneName = done++;

	return combinator.first ?
		// Check against closest ancestor/preceding element
		function( elem, context, xml ) {
			while ( (elem = elem[ dir ]) ) {
				if ( elem.nodeType === 1 || checkNonElements ) {
					return matcher( elem, context, xml );
				}
			}
		} :

		// Check against all ancestor/preceding elements
		function( elem, context, xml ) {
			var oldCache, uniqueCache, outerCache,
				newCache = [ dirruns, doneName ];

			// We can't set arbitrary data on XML nodes, so they don't benefit from combinator caching
			if ( xml ) {
				while ( (elem = elem[ dir ]) ) {
					if ( elem.nodeType === 1 || checkNonElements ) {
						if ( matcher( elem, context, xml ) ) {
							return true;
						}
					}
				}
			} else {
				while ( (elem = elem[ dir ]) ) {
					if ( elem.nodeType === 1 || checkNonElements ) {
						outerCache = elem[ expando ] || (elem[ expando ] = {});

						// Support: IE <9 only
						// Defend against cloned attroperties (jQuery gh-1709)
						uniqueCache = outerCache[ elem.uniqueID ] || (outerCache[ elem.uniqueID ] = {});

						if ( (oldCache = uniqueCache[ dir ]) &&
							oldCache[ 0 ] === dirruns && oldCache[ 1 ] === doneName ) {

							// Assign to newCache so results back-propagate to previous elements
							return (newCache[ 2 ] = oldCache[ 2 ]);
						} else {
							// Reuse newcache so results back-propagate to previous elements
							uniqueCache[ dir ] = newCache;

							// A match means we're done; a fail means we have to keep checking
							if ( (newCache[ 2 ] = matcher( elem, context, xml )) ) {
								return true;
							}
						}
					}
				}
			}
		};
}

function elementMatcher( matchers ) {
	return matchers.length > 1 ?
		function( elem, context, xml ) {
			var i = matchers.length;
			while ( i-- ) {
				if ( !matchers[i]( elem, context, xml ) ) {
					return false;
				}
			}
			return true;
		} :
		matchers[0];
}

function multipleContexts( selector, contexts, results ) {
	var i = 0,
		len = contexts.length;
	for ( ; i < len; i++ ) {
		Sizzle( selector, contexts[i], results );
	}
	return results;
}

function condense( unmatched, map, filter, context, xml ) {
	var elem,
		newUnmatched = [],
		i = 0,
		len = unmatched.length,
		mapped = map != null;

	for ( ; i < len; i++ ) {
		if ( (elem = unmatched[i]) ) {
			if ( !filter || filter( elem, context, xml ) ) {
				newUnmatched.push( elem );
				if ( mapped ) {
					map.push( i );
				}
			}
		}
	}

	return newUnmatched;
}

function setMatcher( preFilter, selector, matcher, postFilter, postFinder, postSelector ) {
	if ( postFilter && !postFilter[ expando ] ) {
		postFilter = setMatcher( postFilter );
	}
	if ( postFinder && !postFinder[ expando ] ) {
		postFinder = setMatcher( postFinder, postSelector );
	}
	return markFunction(function( seed, results, context, xml ) {
		var temp, i, elem,
			preMap = [],
			postMap = [],
			preexisting = results.length,

			// Get initial elements from seed or context
			elems = seed || multipleContexts( selector || "*", context.nodeType ? [ context ] : context, [] ),

			// Prefilter to get matcher input, preserving a map for seed-results synchronization
			matcherIn = preFilter && ( seed || !selector ) ?
				condense( elems, preMap, preFilter, context, xml ) :
				elems,

			matcherOut = matcher ?
				// If we have a postFinder, or filtered seed, or non-seed postFilter or preexisting results,
				postFinder || ( seed ? preFilter : preexisting || postFilter ) ?

					// ...intermediate processing is necessary
					[] :

					// ...otherwise use results directly
					results :
				matcherIn;

		// Find primary matches
		if ( matcher ) {
			matcher( matcherIn, matcherOut, context, xml );
		}

		// Apply postFilter
		if ( postFilter ) {
			temp = condense( matcherOut, postMap );
			postFilter( temp, [], context, xml );

			// Un-match failing elements by moving them back to matcherIn
			i = temp.length;
			while ( i-- ) {
				if ( (elem = temp[i]) ) {
					matcherOut[ postMap[i] ] = !(matcherIn[ postMap[i] ] = elem);
				}
			}
		}

		if ( seed ) {
			if ( postFinder || preFilter ) {
				if ( postFinder ) {
					// Get the final matcherOut by condensing this intermediate into postFinder contexts
					temp = [];
					i = matcherOut.length;
					while ( i-- ) {
						if ( (elem = matcherOut[i]) ) {
							// Restore matcherIn since elem is not yet a final match
							temp.push( (matcherIn[i] = elem) );
						}
					}
					postFinder( null, (matcherOut = []), temp, xml );
				}

				// Move matched elements from seed to results to keep them synchronized
				i = matcherOut.length;
				while ( i-- ) {
					if ( (elem = matcherOut[i]) &&
						(temp = postFinder ? indexOf( seed, elem ) : preMap[i]) > -1 ) {

						seed[temp] = !(results[temp] = elem);
					}
				}
			}

		// Add elements to results, through postFinder if defined
		} else {
			matcherOut = condense(
				matcherOut === results ?
					matcherOut.splice( preexisting, matcherOut.length ) :
					matcherOut
			);
			if ( postFinder ) {
				postFinder( null, results, matcherOut, xml );
			} else {
				push.apply( results, matcherOut );
			}
		}
	});
}

function matcherFromTokens( tokens ) {
	var checkContext, matcher, j,
		len = tokens.length,
		leadingRelative = Expr.relative[ tokens[0].type ],
		implicitRelative = leadingRelative || Expr.relative[" "],
		i = leadingRelative ? 1 : 0,

		// The foundational matcher ensures that elements are reachable from top-level context(s)
		matchContext = addCombinator( function( elem ) {
			return elem === checkContext;
		}, implicitRelative, true ),
		matchAnyContext = addCombinator( function( elem ) {
			return indexOf( checkContext, elem ) > -1;
		}, implicitRelative, true ),
		matchers = [ function( elem, context, xml ) {
			var ret = ( !leadingRelative && ( xml || context !== outermostContext ) ) || (
				(checkContext = context).nodeType ?
					matchContext( elem, context, xml ) :
					matchAnyContext( elem, context, xml ) );
			// Avoid hanging onto element (issue #299)
			checkContext = null;
			return ret;
		} ];

	for ( ; i < len; i++ ) {
		if ( (matcher = Expr.relative[ tokens[i].type ]) ) {
			matchers = [ addCombinator(elementMatcher( matchers ), matcher) ];
		} else {
			matcher = Expr.filter[ tokens[i].type ].apply( null, tokens[i].matches );

			// Return special upon seeing a positional matcher
			if ( matcher[ expando ] ) {
				// Find the next relative operator (if any) for proper handling
				j = ++i;
				for ( ; j < len; j++ ) {
					if ( Expr.relative[ tokens[j].type ] ) {
						break;
					}
				}
				return setMatcher(
					i > 1 && elementMatcher( matchers ),
					i > 1 && toSelector(
						// If the preceding token was a descendant combinator, insert an implicit any-element `*`
						tokens.slice( 0, i - 1 ).concat({ value: tokens[ i - 2 ].type === " " ? "*" : "" })
					).replace( rtrim, "$1" ),
					matcher,
					i < j && matcherFromTokens( tokens.slice( i, j ) ),
					j < len && matcherFromTokens( (tokens = tokens.slice( j )) ),
					j < len && toSelector( tokens )
				);
			}
			matchers.push( matcher );
		}
	}

	return elementMatcher( matchers );
}

function matcherFromGroupMatchers( elementMatchers, setMatchers ) {
	var bySet = setMatchers.length > 0,
		byElement = elementMatchers.length > 0,
		superMatcher = function( seed, context, xml, results, outermost ) {
			var elem, j, matcher,
				matchedCount = 0,
				i = "0",
				unmatched = seed && [],
				setMatched = [],
				contextBackup = outermostContext,
				// We must always have either seed elements or outermost context
				elems = seed || byElement && Expr.find["TAG"]( "*", outermost ),
				// Use integer dirruns iff this is the outermost matcher
				dirrunsUnique = (dirruns += contextBackup == null ? 1 : Math.random() || 0.1),
				len = elems.length;

			if ( outermost ) {
				outermostContext = context === document || context || outermost;
			}

			// Add elements passing elementMatchers directly to results
			// Support: IE<9, Safari
			// Tolerate NodeList properties (IE: "length"; Safari: <number>) matching elements by id
			for ( ; i !== len && (elem = elems[i]) != null; i++ ) {
				if ( byElement && elem ) {
					j = 0;
					if ( !context && elem.ownerDocument !== document ) {
						setDocument( elem );
						xml = !documentIsHTML;
					}
					while ( (matcher = elementMatchers[j++]) ) {
						if ( matcher( elem, context || document, xml) ) {
							results.push( elem );
							break;
						}
					}
					if ( outermost ) {
						dirruns = dirrunsUnique;
					}
				}

				// Track unmatched elements for set filters
				if ( bySet ) {
					// They will have gone through all possible matchers
					if ( (elem = !matcher && elem) ) {
						matchedCount--;
					}

					// Lengthen the array for every element, matched or not
					if ( seed ) {
						unmatched.push( elem );
					}
				}
			}

			// `i` is now the count of elements visited above, and adding it to `matchedCount`
			// makes the latter nonnegative.
			matchedCount += i;

			// Apply set filters to unmatched elements
			// NOTE: This can be skipped if there are no unmatched elements (i.e., `matchedCount`
			// equals `i`), unless we didn't visit _any_ elements in the above loop because we have
			// no element matchers and no seed.
			// Incrementing an initially-string "0" `i` allows `i` to remain a string only in that
			// case, which will result in a "00" `matchedCount` that differs from `i` but is also
			// numerically zero.
			if ( bySet && i !== matchedCount ) {
				j = 0;
				while ( (matcher = setMatchers[j++]) ) {
					matcher( unmatched, setMatched, context, xml );
				}

				if ( seed ) {
					// Reintegrate element matches to eliminate the need for sorting
					if ( matchedCount > 0 ) {
						while ( i-- ) {
							if ( !(unmatched[i] || setMatched[i]) ) {
								setMatched[i] = pop.call( results );
							}
						}
					}

					// Discard index placeholder values to get only actual matches
					setMatched = condense( setMatched );
				}

				// Add matches to results
				push.apply( results, setMatched );

				// Seedless set matches succeeding multiple successful matchers stipulate sorting
				if ( outermost && !seed && setMatched.length > 0 &&
					( matchedCount + setMatchers.length ) > 1 ) {

					Sizzle.uniqueSort( results );
				}
			}

			// Override manipulation of globals by nested matchers
			if ( outermost ) {
				dirruns = dirrunsUnique;
				outermostContext = contextBackup;
			}

			return unmatched;
		};

	return bySet ?
		markFunction( superMatcher ) :
		superMatcher;
}

compile = Sizzle.compile = function( selector, match /* Internal Use Only */ ) {
	var i,
		setMatchers = [],
		elementMatchers = [],
		cached = compilerCache[ selector + " " ];

	if ( !cached ) {
		// Generate a function of recursive functions that can be used to check each element
		if ( !match ) {
			match = tokenize( selector );
		}
		i = match.length;
		while ( i-- ) {
			cached = matcherFromTokens( match[i] );
			if ( cached[ expando ] ) {
				setMatchers.push( cached );
			} else {
				elementMatchers.push( cached );
			}
		}

		// Cache the compiled function
		cached = compilerCache( selector, matcherFromGroupMatchers( elementMatchers, setMatchers ) );

		// Save selector and tokenization
		cached.selector = selector;
	}
	return cached;
};

/**
 * A low-level selection function that works with Sizzle's compiled
 *  selector functions
 * @param {String|Function} selector A selector or a pre-compiled
 *  selector function built with Sizzle.compile
 * @param {Element} context
 * @param {Array} [results]
 * @param {Array} [seed] A set of elements to match against
 */
select = Sizzle.select = function( selector, context, results, seed ) {
	var i, tokens, token, type, find,
		compiled = typeof selector === "function" && selector,
		match = !seed && tokenize( (selector = compiled.selector || selector) );

	results = results || [];

	// Try to minimize operations if there is only one selector in the list and no seed
	// (the latter of which guarantees us context)
	if ( match.length === 1 ) {

		// Reduce context if the leading compound selector is an ID
		tokens = match[0] = match[0].slice( 0 );
		if ( tokens.length > 2 && (token = tokens[0]).type === "ID" &&
				support.getById && context.nodeType === 9 && documentIsHTML &&
				Expr.relative[ tokens[1].type ] ) {

			context = ( Expr.find["ID"]( token.matches[0].replace(runescape, funescape), context ) || [] )[0];
			if ( !context ) {
				return results;

			// Precompiled matchers will still verify ancestry, so step up a level
			} else if ( compiled ) {
				context = context.parentNode;
			}

			selector = selector.slice( tokens.shift().value.length );
		}

		// Fetch a seed set for right-to-left matching
		i = matchExpr["needsContext"].test( selector ) ? 0 : tokens.length;
		while ( i-- ) {
			token = tokens[i];

			// Abort if we hit a combinator
			if ( Expr.relative[ (type = token.type) ] ) {
				break;
			}
			if ( (find = Expr.find[ type ]) ) {
				// Search, expanding context for leading sibling combinators
				if ( (seed = find(
					token.matches[0].replace( runescape, funescape ),
					rsibling.test( tokens[0].type ) && testContext( context.parentNode ) || context
				)) ) {

					// If seed is empty or no tokens remain, we can return early
					tokens.splice( i, 1 );
					selector = seed.length && toSelector( tokens );
					if ( !selector ) {
						push.apply( results, seed );
						return results;
					}

					break;
				}
			}
		}
	}

	// Compile and execute a filtering function if one is not provided
	// Provide `match` to avoid retokenization if we modified the selector above
	( compiled || compile( selector, match ) )(
		seed,
		context,
		!documentIsHTML,
		results,
		!context || rsibling.test( selector ) && testContext( context.parentNode ) || context
	);
	return results;
};

// One-time assignments

// Sort stability
support.sortStable = expando.split("").sort( sortOrder ).join("") === expando;

// Support: Chrome 14-35+
// Always assume duplicates if they aren't passed to the comparison function
support.detectDuplicates = !!hasDuplicate;

// Initialize against the default document
setDocument();

// Support: Webkit<537.32 - Safari 6.0.3/Chrome 25 (fixed in Chrome 27)
// Detached nodes confoundingly follow *each other*
support.sortDetached = assert(function( div1 ) {
	// Should return 1, but returns 4 (following)
	return div1.compareDocumentPosition( document.createElement("div") ) & 1;
});

// Support: IE<8
// Prevent attribute/property "interpolation"
// http://msdn.microsoft.com/en-us/library/ms536429%28VS.85%29.aspx
if ( !assert(function( div ) {
	div.innerHTML = "<a href='#'></a>";
	return div.firstChild.getAttribute("href") === "#" ;
}) ) {
	addHandle( "type|href|height|width", function( elem, name, isXML ) {
		if ( !isXML ) {
			return elem.getAttribute( name, name.toLowerCase() === "type" ? 1 : 2 );
		}
	});
}

// Support: IE<9
// Use defaultValue in place of getAttribute("value")
if ( !support.attributes || !assert(function( div ) {
	div.innerHTML = "<input/>";
	div.firstChild.setAttribute( "value", "" );
	return div.firstChild.getAttribute( "value" ) === "";
}) ) {
	addHandle( "value", function( elem, name, isXML ) {
		if ( !isXML && elem.nodeName.toLowerCase() === "input" ) {
			return elem.defaultValue;
		}
	});
}

// Support: IE<9
// Use getAttributeNode to fetch booleans when getAttribute lies
if ( !assert(function( div ) {
	return div.getAttribute("disabled") == null;
}) ) {
	addHandle( booleans, function( elem, name, isXML ) {
		var val;
		if ( !isXML ) {
			return elem[ name ] === true ? name.toLowerCase() :
					(val = elem.getAttributeNode( name )) && val.specified ?
					val.value :
				null;
		}
	});
}

return Sizzle;

})( window );



jQuery.find = Sizzle;
jQuery.expr = Sizzle.selectors;
jQuery.expr[ ":" ] = jQuery.expr.pseudos;
jQuery.uniqueSort = jQuery.unique = Sizzle.uniqueSort;
jQuery.text = Sizzle.getText;
jQuery.isXMLDoc = Sizzle.isXML;
jQuery.contains = Sizzle.contains;



var dir = function( elem, dir, until ) {
	var matched = [],
		truncate = until !== undefined;

	while ( ( elem = elem[ dir ] ) && elem.nodeType !== 9 ) {
		if ( elem.nodeType === 1 ) {
			if ( truncate && jQuery( elem ).is( until ) ) {
				break;
			}
			matched.push( elem );
		}
	}
	return matched;
};


var siblings = function( n, elem ) {
	var matched = [];

	for ( ; n; n = n.nextSibling ) {
		if ( n.nodeType === 1 && n !== elem ) {
			matched.push( n );
		}
	}

	return matched;
};


var rneedsContext = jQuery.expr.match.needsContext;

var rsingleTag = ( /^<([\w-]+)\s*\/?>(?:<\/\1>|)$/ );



var risSimple = /^.[^:#\[\.,]*$/;

// Implement the identical functionality for filter and not
function winnow( elements, qualifier, not ) {
	if ( jQuery.isFunction( qualifier ) ) {
		return jQuery.grep( elements, function( elem, i ) {
			/* jshint -W018 */
			return !!qualifier.call( elem, i, elem ) !== not;
		} );

	}

	if ( qualifier.nodeType ) {
		return jQuery.grep( elements, function( elem ) {
			return ( elem === qualifier ) !== not;
		} );

	}

	if ( typeof qualifier === "string" ) {
		if ( risSimple.test( qualifier ) ) {
			return jQuery.filter( qualifier, elements, not );
		}

		qualifier = jQuery.filter( qualifier, elements );
	}

	return jQuery.grep( elements, function( elem ) {
		return ( jQuery.inArray( elem, qualifier ) > -1 ) !== not;
	} );
}

jQuery.filter = function( expr, elems, not ) {
	var elem = elems[ 0 ];

	if ( not ) {
		expr = ":not(" + expr + ")";
	}

	return elems.length === 1 && elem.nodeType === 1 ?
		jQuery.find.matchesSelector( elem, expr ) ? [ elem ] : [] :
		jQuery.find.matches( expr, jQuery.grep( elems, function( elem ) {
			return elem.nodeType === 1;
		} ) );
};

jQuery.fn.extend( {
	find: function( selector ) {
		var i,
			ret = [],
			self = this,
			len = self.length;

		if ( typeof selector !== "string" ) {
			return this.pushStack( jQuery( selector ).filter( function() {
				for ( i = 0; i < len; i++ ) {
					if ( jQuery.contains( self[ i ], this ) ) {
						return true;
					}
				}
			} ) );
		}

		for ( i = 0; i < len; i++ ) {
			jQuery.find( selector, self[ i ], ret );
		}

		// Needed because $( selector, context ) becomes $( context ).find( selector )
		ret = this.pushStack( len > 1 ? jQuery.unique( ret ) : ret );
		ret.selector = this.selector ? this.selector + " " + selector : selector;
		return ret;
	},
	filter: function( selector ) {
		return this.pushStack( winnow( this, selector || [], false ) );
	},
	not: function( selector ) {
		return this.pushStack( winnow( this, selector || [], true ) );
	},
	is: function( selector ) {
		return !!winnow(
			this,

			// If this is a positional/relative selector, check membership in the returned set
			// so $("p:first").is("p:last") won't return true for a doc with two "p".
			typeof selector === "string" && rneedsContext.test( selector ) ?
				jQuery( selector ) :
				selector || [],
			false
		).length;
	}
} );


// Initialize a jQuery object


// A central reference to the root jQuery(document)
var rootjQuery,

	// A simple way to check for HTML strings
	// Prioritize #id over <tag> to avoid XSS via location.hash (#9521)
	// Strict HTML recognition (#11290: must start with <)
	rquickExpr = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]*))$/,

	init = jQuery.fn.init = function( selector, context, root ) {
		var match, elem;

		// HANDLE: $(""), $(null), $(undefined), $(false)
		if ( !selector ) {
			return this;
		}

		// init accepts an alternate rootjQuery
		// so migrate can support jQuery.sub (gh-2101)
		root = root || rootjQuery;

		// Handle HTML strings
		if ( typeof selector === "string" ) {
			if ( selector.charAt( 0 ) === "<" &&
				selector.charAt( selector.length - 1 ) === ">" &&
				selector.length >= 3 ) {

				// Assume that strings that start and end with <> are HTML and skip the regex check
				match = [ null, selector, null ];

			} else {
				match = rquickExpr.exec( selector );
			}

			// Match html or make sure no context is specified for #id
			if ( match && ( match[ 1 ] || !context ) ) {

				// HANDLE: $(html) -> $(array)
				if ( match[ 1 ] ) {
					context = context instanceof jQuery ? context[ 0 ] : context;

					// scripts is true for back-compat
					// Intentionally let the error be thrown if parseHTML is not present
					jQuery.merge( this, jQuery.parseHTML(
						match[ 1 ],
						context && context.nodeType ? context.ownerDocument || context : document,
						true
					) );

					// HANDLE: $(html, props)
					if ( rsingleTag.test( match[ 1 ] ) && jQuery.isPlainObject( context ) ) {
						for ( match in context ) {

							// Properties of context are called as methods if possible
							if ( jQuery.isFunction( this[ match ] ) ) {
								this[ match ]( context[ match ] );

							// ...and otherwise set as attributes
							} else {
								this.attr( match, context[ match ] );
							}
						}
					}

					return this;

				// HANDLE: $(#id)
				} else {
					elem = document.getElementById( match[ 2 ] );

					// Check parentNode to catch when Blackberry 4.6 returns
					// nodes that are no longer in the document #6963
					if ( elem && elem.parentNode ) {

						// Handle the case where IE and Opera return items
						// by name instead of ID
						if ( elem.id !== match[ 2 ] ) {
							return rootjQuery.find( selector );
						}

						// Otherwise, we inject the element directly into the jQuery object
						this.length = 1;
						this[ 0 ] = elem;
					}

					this.context = document;
					this.selector = selector;
					return this;
				}

			// HANDLE: $(expr, $(...))
			} else if ( !context || context.jquery ) {
				return ( context || root ).find( selector );

			// HANDLE: $(expr, context)
			// (which is just equivalent to: $(context).find(expr)
			} else {
				return this.constructor( context ).find( selector );
			}

		// HANDLE: $(DOMElement)
		} else if ( selector.nodeType ) {
			this.context = this[ 0 ] = selector;
			this.length = 1;
			return this;

		// HANDLE: $(function)
		// Shortcut for document ready
		} else if ( jQuery.isFunction( selector ) ) {
			return typeof root.ready !== "undefined" ?
				root.ready( selector ) :

				// Execute immediately if ready is not present
				selector( jQuery );
		}

		if ( selector.selector !== undefined ) {
			this.selector = selector.selector;
			this.context = selector.context;
		}

		return jQuery.makeArray( selector, this );
	};

// Give the init function the jQuery prototype for later instantiation
init.prototype = jQuery.fn;

// Initialize central reference
rootjQuery = jQuery( document );


var rparentsprev = /^(?:parents|prev(?:Until|All))/,

	// methods guaranteed to produce a unique set when starting from a unique set
	guaranteedUnique = {
		children: true,
		contents: true,
		next: true,
		prev: true
	};

jQuery.fn.extend( {
	has: function( target ) {
		var i,
			targets = jQuery( target, this ),
			len = targets.length;

		return this.filter( function() {
			for ( i = 0; i < len; i++ ) {
				if ( jQuery.contains( this, targets[ i ] ) ) {
					return true;
				}
			}
		} );
	},

	closest: function( selectors, context ) {
		var cur,
			i = 0,
			l = this.length,
			matched = [],
			pos = rneedsContext.test( selectors ) || typeof selectors !== "string" ?
				jQuery( selectors, context || this.context ) :
				0;

		for ( ; i < l; i++ ) {
			for ( cur = this[ i ]; cur && cur !== context; cur = cur.parentNode ) {

				// Always skip document fragments
				if ( cur.nodeType < 11 && ( pos ?
					pos.index( cur ) > -1 :

					// Don't pass non-elements to Sizzle
					cur.nodeType === 1 &&
						jQuery.find.matchesSelector( cur, selectors ) ) ) {

					matched.push( cur );
					break;
				}
			}
		}

		return this.pushStack( matched.length > 1 ? jQuery.uniqueSort( matched ) : matched );
	},

	// Determine the position of an element within
	// the matched set of elements
	index: function( elem ) {

		// No argument, return index in parent
		if ( !elem ) {
			return ( this[ 0 ] && this[ 0 ].parentNode ) ? this.first().prevAll().length : -1;
		}

		// index in selector
		if ( typeof elem === "string" ) {
			return jQuery.inArray( this[ 0 ], jQuery( elem ) );
		}

		// Locate the position of the desired element
		return jQuery.inArray(

			// If it receives a jQuery object, the first element is used
			elem.jquery ? elem[ 0 ] : elem, this );
	},

	add: function( selector, context ) {
		return this.pushStack(
			jQuery.uniqueSort(
				jQuery.merge( this.get(), jQuery( selector, context ) )
			)
		);
	},

	addBack: function( selector ) {
		return this.add( selector == null ?
			this.prevObject : this.prevObject.filter( selector )
		);
	}
} );

function sibling( cur, dir ) {
	do {
		cur = cur[ dir ];
	} while ( cur && cur.nodeType !== 1 );

	return cur;
}

jQuery.each( {
	parent: function( elem ) {
		var parent = elem.parentNode;
		return parent && parent.nodeType !== 11 ? parent : null;
	},
	parents: function( elem ) {
		return dir( elem, "parentNode" );
	},
	parentsUntil: function( elem, i, until ) {
		return dir( elem, "parentNode", until );
	},
	next: function( elem ) {
		return sibling( elem, "nextSibling" );
	},
	prev: function( elem ) {
		return sibling( elem, "previousSibling" );
	},
	nextAll: function( elem ) {
		return dir( elem, "nextSibling" );
	},
	prevAll: function( elem ) {
		return dir( elem, "previousSibling" );
	},
	nextUntil: function( elem, i, until ) {
		return dir( elem, "nextSibling", until );
	},
	prevUntil: function( elem, i, until ) {
		return dir( elem, "previousSibling", until );
	},
	siblings: function( elem ) {
		return siblings( ( elem.parentNode || {} ).firstChild, elem );
	},
	children: function( elem ) {
		return siblings( elem.firstChild );
	},
	contents: function( elem ) {
		return jQuery.nodeName( elem, "iframe" ) ?
			elem.contentDocument || elem.contentWindow.document :
			jQuery.merge( [], elem.childNodes );
	}
}, function( name, fn ) {
	jQuery.fn[ name ] = function( until, selector ) {
		var ret = jQuery.map( this, fn, until );

		if ( name.slice( -5 ) !== "Until" ) {
			selector = until;
		}

		if ( selector && typeof selector === "string" ) {
			ret = jQuery.filter( selector, ret );
		}

		if ( this.length > 1 ) {

			// Remove duplicates
			if ( !guaranteedUnique[ name ] ) {
				ret = jQuery.uniqueSort( ret );
			}

			// Reverse order for parents* and prev-derivatives
			if ( rparentsprev.test( name ) ) {
				ret = ret.reverse();
			}
		}

		return this.pushStack( ret );
	};
} );
var rnotwhite = ( /\S+/g );



// Convert String-formatted options into Object-formatted ones
function createOptions( options ) {
	var object = {};
	jQuery.each( options.match( rnotwhite ) || [], function( _, flag ) {
		object[ flag ] = true;
	} );
	return object;
}

/*
 * Create a callback list using the following parameters:
 *
 *	options: an optional list of space-separated options that will change how
 *			the callback list behaves or a more traditional option object
 *
 * By default a callback list will act like an event callback list and can be
 * "fired" multiple times.
 *
 * Possible options:
 *
 *	once:			will ensure the callback list can only be fired once (like a Deferred)
 *
 *	memory:			will keep track of previous values and will call any callback added
 *					after the list has been fired right away with the latest "memorized"
 *					values (like a Deferred)
 *
 *	unique:			will ensure a callback can only be added once (no duplicate in the list)
 *
 *	stopOnFalse:	interrupt callings when a callback returns false
 *
 */
jQuery.Callbacks = function( options ) {

	// Convert options from String-formatted to Object-formatted if needed
	// (we check in cache first)
	options = typeof options === "string" ?
		createOptions( options ) :
		jQuery.extend( {}, options );

	var // Flag to know if list is currently firing
		firing,

		// Last fire value for non-forgettable lists
		memory,

		// Flag to know if list was already fired
		fired,

		// Flag to prevent firing
		locked,

		// Actual callback list
		list = [],

		// Queue of execution data for repeatable lists
		queue = [],

		// Index of currently firing callback (modified by add/remove as needed)
		firingIndex = -1,

		// Fire callbacks
		fire = function() {

			// Enforce single-firing
			locked = options.once;

			// Execute callbacks for all pending executions,
			// respecting firingIndex overrides and runtime changes
			fired = firing = true;
			for ( ; queue.length; firingIndex = -1 ) {
				memory = queue.shift();
				while ( ++firingIndex < list.length ) {

					// Run callback and check for early termination
					if ( list[ firingIndex ].apply( memory[ 0 ], memory[ 1 ] ) === false &&
						options.stopOnFalse ) {

						// Jump to end and forget the data so .add doesn't re-fire
						firingIndex = list.length;
						memory = false;
					}
				}
			}

			// Forget the data if we're done with it
			if ( !options.memory ) {
				memory = false;
			}

			firing = false;

			// Clean up if we're done firing for good
			if ( locked ) {

				// Keep an empty list if we have data for future add calls
				if ( memory ) {
					list = [];

				// Otherwise, this object is spent
				} else {
					list = "";
				}
			}
		},

		// Actual Callbacks object
		self = {

			// Add a callback or a collection of callbacks to the list
			add: function() {
				if ( list ) {

					// If we have memory from a past run, we should fire after adding
					if ( memory && !firing ) {
						firingIndex = list.length - 1;
						queue.push( memory );
					}

					( function add( args ) {
						jQuery.each( args, function( _, arg ) {
							if ( jQuery.isFunction( arg ) ) {
								if ( !options.unique || !self.has( arg ) ) {
									list.push( arg );
								}
							} else if ( arg && arg.length && jQuery.type( arg ) !== "string" ) {

								// Inspect recursively
								add( arg );
							}
						} );
					} )( arguments );

					if ( memory && !firing ) {
						fire();
					}
				}
				return this;
			},

			// Remove a callback from the list
			remove: function() {
				jQuery.each( arguments, function( _, arg ) {
					var index;
					while ( ( index = jQuery.inArray( arg, list, index ) ) > -1 ) {
						list.splice( index, 1 );

						// Handle firing indexes
						if ( index <= firingIndex ) {
							firingIndex--;
						}
					}
				} );
				return this;
			},

			// Check if a given callback is in the list.
			// If no argument is given, return whether or not list has callbacks attached.
			has: function( fn ) {
				return fn ?
					jQuery.inArray( fn, list ) > -1 :
					list.length > 0;
			},

			// Remove all callbacks from the list
			empty: function() {
				if ( list ) {
					list = [];
				}
				return this;
			},

			// Disable .fire and .add
			// Abort any current/pending executions
			// Clear all callbacks and values
			disable: function() {
				locked = queue = [];
				list = memory = "";
				return this;
			},
			disabled: function() {
				return !list;
			},

			// Disable .fire
			// Also disable .add unless we have memory (since it would have no effect)
			// Abort any pending executions
			lock: function() {
				locked = true;
				if ( !memory ) {
					self.disable();
				}
				return this;
			},
			locked: function() {
				return !!locked;
			},

			// Call all callbacks with the given context and arguments
			fireWith: function( context, args ) {
				if ( !locked ) {
					args = args || [];
					args = [ context, args.slice ? args.slice() : args ];
					queue.push( args );
					if ( !firing ) {
						fire();
					}
				}
				return this;
			},

			// Call all the callbacks with the given arguments
			fire: function() {
				self.fireWith( this, arguments );
				return this;
			},

			// To know if the callbacks have already been called at least once
			fired: function() {
				return !!fired;
			}
		};

	return self;
};


jQuery.extend( {

	Deferred: function( func ) {
		var tuples = [

				// action, add listener, listener list, final state
				[ "resolve", "done", jQuery.Callbacks( "once memory" ), "resolved" ],
				[ "reject", "fail", jQuery.Callbacks( "once memory" ), "rejected" ],
				[ "notify", "progress", jQuery.Callbacks( "memory" ) ]
			],
			state = "pending",
			promise = {
				state: function() {
					return state;
				},
				always: function() {
					deferred.done( arguments ).fail( arguments );
					return this;
				},
				then: function( /* fnDone, fnFail, fnProgress */ ) {
					var fns = arguments;
					return jQuery.Deferred( function( newDefer ) {
						jQuery.each( tuples, function( i, tuple ) {
							var fn = jQuery.isFunction( fns[ i ] ) && fns[ i ];

							// deferred[ done | fail | progress ] for forwarding actions to newDefer
							deferred[ tuple[ 1 ] ]( function() {
								var returned = fn && fn.apply( this, arguments );
								if ( returned && jQuery.isFunction( returned.promise ) ) {
									returned.promise()
										.progress( newDefer.notify )
										.done( newDefer.resolve )
										.fail( newDefer.reject );
								} else {
									newDefer[ tuple[ 0 ] + "With" ](
										this === promise ? newDefer.promise() : this,
										fn ? [ returned ] : arguments
									);
								}
							} );
						} );
						fns = null;
					} ).promise();
				},

				// Get a promise for this deferred
				// If obj is provided, the promise aspect is added to the object
				promise: function( obj ) {
					return obj != null ? jQuery.extend( obj, promise ) : promise;
				}
			},
			deferred = {};

		// Keep pipe for back-compat
		promise.pipe = promise.then;

		// Add list-specific methods
		jQuery.each( tuples, function( i, tuple ) {
			var list = tuple[ 2 ],
				stateString = tuple[ 3 ];

			// promise[ done | fail | progress ] = list.add
			promise[ tuple[ 1 ] ] = list.add;

			// Handle state
			if ( stateString ) {
				list.add( function() {

					// state = [ resolved | rejected ]
					state = stateString;

				// [ reject_list | resolve_list ].disable; progress_list.lock
				}, tuples[ i ^ 1 ][ 2 ].disable, tuples[ 2 ][ 2 ].lock );
			}

			// deferred[ resolve | reject | notify ]
			deferred[ tuple[ 0 ] ] = function() {
				deferred[ tuple[ 0 ] + "With" ]( this === deferred ? promise : this, arguments );
				return this;
			};
			deferred[ tuple[ 0 ] + "With" ] = list.fireWith;
		} );

		// Make the deferred a promise
		promise.promise( deferred );

		// Call given func if any
		if ( func ) {
			func.call( deferred, deferred );
		}

		// All done!
		return deferred;
	},

	// Deferred helper
	when: function( subordinate /* , ..., subordinateN */ ) {
		var i = 0,
			resolveValues = slice.call( arguments ),
			length = resolveValues.length,

			// the count of uncompleted subordinates
			remaining = length !== 1 ||
				( subordinate && jQuery.isFunction( subordinate.promise ) ) ? length : 0,

			// the master Deferred.
			// If resolveValues consist of only a single Deferred, just use that.
			deferred = remaining === 1 ? subordinate : jQuery.Deferred(),

			// Update function for both resolve and progress values
			updateFunc = function( i, contexts, values ) {
				return function( value ) {
					contexts[ i ] = this;
					values[ i ] = arguments.length > 1 ? slice.call( arguments ) : value;
					if ( values === progressValues ) {
						deferred.notifyWith( contexts, values );

					} else if ( !( --remaining ) ) {
						deferred.resolveWith( contexts, values );
					}
				};
			},

			progressValues, progressContexts, resolveContexts;

		// add listeners to Deferred subordinates; treat others as resolved
		if ( length > 1 ) {
			progressValues = new Array( length );
			progressContexts = new Array( length );
			resolveContexts = new Array( length );
			for ( ; i < length; i++ ) {
				if ( resolveValues[ i ] && jQuery.isFunction( resolveValues[ i ].promise ) ) {
					resolveValues[ i ].promise()
						.progress( updateFunc( i, progressContexts, progressValues ) )
						.done( updateFunc( i, resolveContexts, resolveValues ) )
						.fail( deferred.reject );
				} else {
					--remaining;
				}
			}
		}

		// if we're not waiting on anything, resolve the master
		if ( !remaining ) {
			deferred.resolveWith( resolveContexts, resolveValues );
		}

		return deferred.promise();
	}
} );


// The deferred used on DOM ready
var readyList;

jQuery.fn.ready = function( fn ) {

	// Add the callback
	jQuery.ready.promise().done( fn );

	return this;
};

jQuery.extend( {

	// Is the DOM ready to be used? Set to true once it occurs.
	isReady: false,

	// A counter to track how many items to wait for before
	// the ready event fires. See #6781
	readyWait: 1,

	// Hold (or release) the ready event
	holdReady: function( hold ) {
		if ( hold ) {
			jQuery.readyWait++;
		} else {
			jQuery.ready( true );
		}
	},

	// Handle when the DOM is ready
	ready: function( wait ) {

		// Abort if there are pending holds or we're already ready
		if ( wait === true ? --jQuery.readyWait : jQuery.isReady ) {
			return;
		}

		// Remember that the DOM is ready
		jQuery.isReady = true;

		// If a normal DOM Ready event fired, decrement, and wait if need be
		if ( wait !== true && --jQuery.readyWait > 0 ) {
			return;
		}

		// If there are functions bound, to execute
		readyList.resolveWith( document, [ jQuery ] );

		// Trigger any bound ready events
		if ( jQuery.fn.triggerHandler ) {
			jQuery( document ).triggerHandler( "ready" );
			jQuery( document ).off( "ready" );
		}
	}
} );

/**
 * Clean-up method for dom ready events
 */
function detach() {
	if ( document.addEventListener ) {
		document.removeEventListener( "DOMContentLoaded", completed );
		window.removeEventListener( "load", completed );

	} else {
		document.detachEvent( "onreadystatechange", completed );
		window.detachEvent( "onload", completed );
	}
}

/**
 * The ready event handler and self cleanup method
 */
function completed() {

	// readyState === "complete" is good enough for us to call the dom ready in oldIE
	if ( document.addEventListener ||
		window.event.type === "load" ||
		document.readyState === "complete" ) {

		detach();
		jQuery.ready();
	}
}

jQuery.ready.promise = function( obj ) {
	if ( !readyList ) {

		readyList = jQuery.Deferred();

		// Catch cases where $(document).ready() is called
		// after the browser event has already occurred.
		// we once tried to use readyState "interactive" here,
		// but it caused issues like the one
		// discovered by ChrisS here:
		// http://bugs.jquery.com/ticket/12282#comment:15
		if ( document.readyState === "complete" ) {

			// Handle it asynchronously to allow scripts the opportunity to delay ready
			window.setTimeout( jQuery.ready );

		// Standards-based browsers support DOMContentLoaded
		} else if ( document.addEventListener ) {

			// Use the handy event callback
			document.addEventListener( "DOMContentLoaded", completed );

			// A fallback to window.onload, that will always work
			window.addEventListener( "load", completed );

		// If IE event model is used
		} else {

			// Ensure firing before onload, maybe late but safe also for iframes
			document.attachEvent( "onreadystatechange", completed );

			// A fallback to window.onload, that will always work
			window.attachEvent( "onload", completed );

			// If IE and not a frame
			// continually check to see if the document is ready
			var top = false;

			try {
				top = window.frameElement == null && document.documentElement;
			} catch ( e ) {}

			if ( top && top.doScroll ) {
				( function doScrollCheck() {
					if ( !jQuery.isReady ) {

						try {

							// Use the trick by Diego Perini
							// http://javascript.nwbox.com/IEContentLoaded/
							top.doScroll( "left" );
						} catch ( e ) {
							return window.setTimeout( doScrollCheck, 50 );
						}

						// detach all dom ready events
						detach();

						// and execute any waiting functions
						jQuery.ready();
					}
				} )();
			}
		}
	}
	return readyList.promise( obj );
};

// Kick off the DOM ready check even if the user does not
jQuery.ready.promise();




// Support: IE<9
// Iteration over object's inherited properties before its own
var i;
for ( i in jQuery( support ) ) {
	break;
}
support.ownFirst = i === "0";

// Note: most support tests are defined in their respective modules.
// false until the test is run
support.inlineBlockNeedsLayout = false;

// Execute ASAP in case we need to set body.style.zoom
jQuery( function() {

	// Minified: var a,b,c,d
	var val, div, body, container;

	body = document.getElementsByTagName( "body" )[ 0 ];
	if ( !body || !body.style ) {

		// Return for frameset docs that don't have a body
		return;
	}

	// Setup
	div = document.createElement( "div" );
	container = document.createElement( "div" );
	container.style.cssText = "position:absolute;border:0;width:0;height:0;top:0;left:-9999px";
	body.appendChild( container ).appendChild( div );

	if ( typeof div.style.zoom !== "undefined" ) {

		// Support: IE<8
		// Check if natively block-level elements act like inline-block
		// elements when setting their display to 'inline' and giving
		// them layout
		div.style.cssText = "display:inline;margin:0;border:0;padding:1px;width:1px;zoom:1";

		support.inlineBlockNeedsLayout = val = div.offsetWidth === 3;
		if ( val ) {

			// Prevent IE 6 from affecting layout for positioned elements #11048
			// Prevent IE from shrinking the body in IE 7 mode #12869
			// Support: IE<8
			body.style.zoom = 1;
		}
	}

	body.removeChild( container );
} );


( function() {
	var div = document.createElement( "div" );

	// Support: IE<9
	support.deleteExpando = true;
	try {
		delete div.test;
	} catch ( e ) {
		support.deleteExpando = false;
	}

	// Null elements to avoid leaks in IE.
	div = null;
} )();
var acceptData = function( elem ) {
	var noData = jQuery.noData[ ( elem.nodeName + " " ).toLowerCase() ],
		nodeType = +elem.nodeType || 1;

	// Do not set data on non-element DOM nodes because it will not be cleared (#8335).
	return nodeType !== 1 && nodeType !== 9 ?
		false :

		// Nodes accept data unless otherwise specified; rejection can be conditional
		!noData || noData !== true && elem.getAttribute( "classid" ) === noData;
};




var rbrace = /^(?:\{[\w\W]*\}|\[[\w\W]*\])$/,
	rmultiDash = /([A-Z])/g;

function dataAttr( elem, key, data ) {

	// If nothing was found internally, try to fetch any
	// data from the HTML5 data-* attribute
	if ( data === undefined && elem.nodeType === 1 ) {

		var name = "data-" + key.replace( rmultiDash, "-$1" ).toLowerCase();

		data = elem.getAttribute( name );

		if ( typeof data === "string" ) {
			try {
				data = data === "true" ? true :
					data === "false" ? false :
					data === "null" ? null :

					// Only convert to a number if it doesn't change the string
					+data + "" === data ? +data :
					rbrace.test( data ) ? jQuery.parseJSON( data ) :
					data;
			} catch ( e ) {}

			// Make sure we set the data so it isn't changed later
			jQuery.data( elem, key, data );

		} else {
			data = undefined;
		}
	}

	return data;
}

// checks a cache object for emptiness
function isEmptyDataObject( obj ) {
	var name;
	for ( name in obj ) {

		// if the public data object is empty, the private is still empty
		if ( name === "data" && jQuery.isEmptyObject( obj[ name ] ) ) {
			continue;
		}
		if ( name !== "toJSON" ) {
			return false;
		}
	}

	return true;
}

function internalData( elem, name, data, pvt /* Internal Use Only */ ) {
	if ( !acceptData( elem ) ) {
		return;
	}

	var ret, thisCache,
		internalKey = jQuery.expando,

		// We have to handle DOM nodes and JS objects differently because IE6-7
		// can't GC object references properly across the DOM-JS boundary
		isNode = elem.nodeType,

		// Only DOM nodes need the global jQuery cache; JS object data is
		// attached directly to the object so GC can occur automatically
		cache = isNode ? jQuery.cache : elem,

		// Only defining an ID for JS objects if its cache already exists allows
		// the code to shortcut on the same path as a DOM node with no cache
		id = isNode ? elem[ internalKey ] : elem[ internalKey ] && internalKey;

	// Avoid doing any more work than we need to when trying to get data on an
	// object that has no data at all
	if ( ( !id || !cache[ id ] || ( !pvt && !cache[ id ].data ) ) &&
		data === undefined && typeof name === "string" ) {
		return;
	}

	if ( !id ) {

		// Only DOM nodes need a new unique ID for each element since their data
		// ends up in the global cache
		if ( isNode ) {
			id = elem[ internalKey ] = deletedIds.pop() || jQuery.guid++;
		} else {
			id = internalKey;
		}
	}

	if ( !cache[ id ] ) {

		// Avoid exposing jQuery metadata on plain JS objects when the object
		// is serialized using JSON.stringify
		cache[ id ] = isNode ? {} : { toJSON: jQuery.noop };
	}

	// An object can be passed to jQuery.data instead of a key/value pair; this gets
	// shallow copied over onto the existing cache
	if ( typeof name === "object" || typeof name === "function" ) {
		if ( pvt ) {
			cache[ id ] = jQuery.extend( cache[ id ], name );
		} else {
			cache[ id ].data = jQuery.extend( cache[ id ].data, name );
		}
	}

	thisCache = cache[ id ];

	// jQuery data() is stored in a separate object inside the object's internal data
	// cache in order to avoid key collisions between internal data and user-defined
	// data.
	if ( !pvt ) {
		if ( !thisCache.data ) {
			thisCache.data = {};
		}

		thisCache = thisCache.data;
	}

	if ( data !== undefined ) {
		thisCache[ jQuery.camelCase( name ) ] = data;
	}

	// Check for both converted-to-camel and non-converted data property names
	// If a data property was specified
	if ( typeof name === "string" ) {

		// First Try to find as-is property data
		ret = thisCache[ name ];

		// Test for null|undefined property data
		if ( ret == null ) {

			// Try to find the camelCased property
			ret = thisCache[ jQuery.camelCase( name ) ];
		}
	} else {
		ret = thisCache;
	}

	return ret;
}

function internalRemoveData( elem, name, pvt ) {
	if ( !acceptData( elem ) ) {
		return;
	}

	var thisCache, i,
		isNode = elem.nodeType,

		// See jQuery.data for more information
		cache = isNode ? jQuery.cache : elem,
		id = isNode ? elem[ jQuery.expando ] : jQuery.expando;

	// If there is already no cache entry for this object, there is no
	// purpose in continuing
	if ( !cache[ id ] ) {
		return;
	}

	if ( name ) {

		thisCache = pvt ? cache[ id ] : cache[ id ].data;

		if ( thisCache ) {

			// Support array or space separated string names for data keys
			if ( !jQuery.isArray( name ) ) {

				// try the string as a key before any manipulation
				if ( name in thisCache ) {
					name = [ name ];
				} else {

					// split the camel cased version by spaces unless a key with the spaces exists
					name = jQuery.camelCase( name );
					if ( name in thisCache ) {
						name = [ name ];
					} else {
						name = name.split( " " );
					}
				}
			} else {

				// If "name" is an array of keys...
				// When data is initially created, via ("key", "val") signature,
				// keys will be converted to camelCase.
				// Since there is no way to tell _how_ a key was added, remove
				// both plain key and camelCase key. #12786
				// This will only penalize the array argument path.
				name = name.concat( jQuery.map( name, jQuery.camelCase ) );
			}

			i = name.length;
			while ( i-- ) {
				delete thisCache[ name[ i ] ];
			}

			// If there is no data left in the cache, we want to continue
			// and let the cache object itself get destroyed
			if ( pvt ? !isEmptyDataObject( thisCache ) : !jQuery.isEmptyObject( thisCache ) ) {
				return;
			}
		}
	}

	// See jQuery.data for more information
	if ( !pvt ) {
		delete cache[ id ].data;

		// Don't destroy the parent cache unless the internal data object
		// had been the only thing left in it
		if ( !isEmptyDataObject( cache[ id ] ) ) {
			return;
		}
	}

	// Destroy the cache
	if ( isNode ) {
		jQuery.cleanData( [ elem ], true );

	// Use delete when supported for expandos or `cache` is not a window per isWindow (#10080)
	/* jshint eqeqeq: false */
	} else if ( support.deleteExpando || cache != cache.window ) {
		/* jshint eqeqeq: true */
		delete cache[ id ];

	// When all else fails, undefined
	} else {
		cache[ id ] = undefined;
	}
}

jQuery.extend( {
	cache: {},

	// The following elements (space-suffixed to avoid Object.prototype collisions)
	// throw uncatchable exceptions if you attempt to set expando properties
	noData: {
		"applet ": true,
		"embed ": true,

		// ...but Flash objects (which have this classid) *can* handle expandos
		"object ": "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
	},

	hasData: function( elem ) {
		elem = elem.nodeType ? jQuery.cache[ elem[ jQuery.expando ] ] : elem[ jQuery.expando ];
		return !!elem && !isEmptyDataObject( elem );
	},

	data: function( elem, name, data ) {
		return internalData( elem, name, data );
	},

	removeData: function( elem, name ) {
		return internalRemoveData( elem, name );
	},

	// For internal use only.
	_data: function( elem, name, data ) {
		return internalData( elem, name, data, true );
	},

	_removeData: function( elem, name ) {
		return internalRemoveData( elem, name, true );
	}
} );

jQuery.fn.extend( {
	data: function( key, value ) {
		var i, name, data,
			elem = this[ 0 ],
			attrs = elem && elem.attributes;

		// Special expections of .data basically thwart jQuery.access,
		// so implement the relevant behavior ourselves

		// Gets all values
		if ( key === undefined ) {
			if ( this.length ) {
				data = jQuery.data( elem );

				if ( elem.nodeType === 1 && !jQuery._data( elem, "parsedAttrs" ) ) {
					i = attrs.length;
					while ( i-- ) {

						// Support: IE11+
						// The attrs elements can be null (#14894)
						if ( attrs[ i ] ) {
							name = attrs[ i ].name;
							if ( name.indexOf( "data-" ) === 0 ) {
								name = jQuery.camelCase( name.slice( 5 ) );
								dataAttr( elem, name, data[ name ] );
							}
						}
					}
					jQuery._data( elem, "parsedAttrs", true );
				}
			}

			return data;
		}

		// Sets multiple values
		if ( typeof key === "object" ) {
			return this.each( function() {
				jQuery.data( this, key );
			} );
		}

		return arguments.length > 1 ?

			// Sets one value
			this.each( function() {
				jQuery.data( this, key, value );
			} ) :

			// Gets one value
			// Try to fetch any internally stored data first
			elem ? dataAttr( elem, key, jQuery.data( elem, key ) ) : undefined;
	},

	removeData: function( key ) {
		return this.each( function() {
			jQuery.removeData( this, key );
		} );
	}
} );


jQuery.extend( {
	queue: function( elem, type, data ) {
		var queue;

		if ( elem ) {
			type = ( type || "fx" ) + "queue";
			queue = jQuery._data( elem, type );

			// Speed up dequeue by getting out quickly if this is just a lookup
			if ( data ) {
				if ( !queue || jQuery.isArray( data ) ) {
					queue = jQuery._data( elem, type, jQuery.makeArray( data ) );
				} else {
					queue.push( data );
				}
			}
			return queue || [];
		}
	},

	dequeue: function( elem, type ) {
		type = type || "fx";

		var queue = jQuery.queue( elem, type ),
			startLength = queue.length,
			fn = queue.shift(),
			hooks = jQuery._queueHooks( elem, type ),
			next = function() {
				jQuery.dequeue( elem, type );
			};

		// If the fx queue is dequeued, always remove the progress sentinel
		if ( fn === "inprogress" ) {
			fn = queue.shift();
			startLength--;
		}

		if ( fn ) {

			// Add a progress sentinel to prevent the fx queue from being
			// automatically dequeued
			if ( type === "fx" ) {
				queue.unshift( "inprogress" );
			}

			// clear up the last queue stop function
			delete hooks.stop;
			fn.call( elem, next, hooks );
		}

		if ( !startLength && hooks ) {
			hooks.empty.fire();
		}
	},

	// not intended for public consumption - generates a queueHooks object,
	// or returns the current one
	_queueHooks: function( elem, type ) {
		var key = type + "queueHooks";
		return jQuery._data( elem, key ) || jQuery._data( elem, key, {
			empty: jQuery.Callbacks( "once memory" ).add( function() {
				jQuery._removeData( elem, type + "queue" );
				jQuery._removeData( elem, key );
			} )
		} );
	}
} );

jQuery.fn.extend( {
	queue: function( type, data ) {
		var setter = 2;

		if ( typeof type !== "string" ) {
			data = type;
			type = "fx";
			setter--;
		}

		if ( arguments.length < setter ) {
			return jQuery.queue( this[ 0 ], type );
		}

		return data === undefined ?
			this :
			this.each( function() {
				var queue = jQuery.queue( this, type, data );

				// ensure a hooks for this queue
				jQuery._queueHooks( this, type );

				if ( type === "fx" && queue[ 0 ] !== "inprogress" ) {
					jQuery.dequeue( this, type );
				}
			} );
	},
	dequeue: function( type ) {
		return this.each( function() {
			jQuery.dequeue( this, type );
		} );
	},
	clearQueue: function( type ) {
		return this.queue( type || "fx", [] );
	},

	// Get a promise resolved when queues of a certain type
	// are emptied (fx is the type by default)
	promise: function( type, obj ) {
		var tmp,
			count = 1,
			defer = jQuery.Deferred(),
			elements = this,
			i = this.length,
			resolve = function() {
				if ( !( --count ) ) {
					defer.resolveWith( elements, [ elements ] );
				}
			};

		if ( typeof type !== "string" ) {
			obj = type;
			type = undefined;
		}
		type = type || "fx";

		while ( i-- ) {
			tmp = jQuery._data( elements[ i ], type + "queueHooks" );
			if ( tmp && tmp.empty ) {
				count++;
				tmp.empty.add( resolve );
			}
		}
		resolve();
		return defer.promise( obj );
	}
} );


( function() {
	var shrinkWrapBlocksVal;

	support.shrinkWrapBlocks = function() {
		if ( shrinkWrapBlocksVal != null ) {
			return shrinkWrapBlocksVal;
		}

		// Will be changed later if needed.
		shrinkWrapBlocksVal = false;

		// Minified: var b,c,d
		var div, body, container;

		body = document.getElementsByTagName( "body" )[ 0 ];
		if ( !body || !body.style ) {

			// Test fired too early or in an unsupported environment, exit.
			return;
		}

		// Setup
		div = document.createElement( "div" );
		container = document.createElement( "div" );
		container.style.cssText = "position:absolute;border:0;width:0;height:0;top:0;left:-9999px";
		body.appendChild( container ).appendChild( div );

		// Support: IE6
		// Check if elements with layout shrink-wrap their children
		if ( typeof div.style.zoom !== "undefined" ) {

			// Reset CSS: box-sizing; display; margin; border
			div.style.cssText =

				// Support: Firefox<29, Android 2.3
				// Vendor-prefix box-sizing
				"-webkit-box-sizing:content-box;-moz-box-sizing:content-box;" +
				"box-sizing:content-box;display:block;margin:0;border:0;" +
				"padding:1px;width:1px;zoom:1";
			div.appendChild( document.createElement( "div" ) ).style.width = "5px";
			shrinkWrapBlocksVal = div.offsetWidth !== 3;
		}

		body.removeChild( container );

		return shrinkWrapBlocksVal;
	};

} )();
var pnum = ( /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/ ).source;

var rcssNum = new RegExp( "^(?:([+-])=|)(" + pnum + ")([a-z%]*)$", "i" );


var cssExpand = [ "Top", "Right", "Bottom", "Left" ];

var isHidden = function( elem, el ) {

		// isHidden might be called from jQuery#filter function;
		// in that case, element will be second argument
		elem = el || elem;
		return jQuery.css( elem, "display" ) === "none" ||
			!jQuery.contains( elem.ownerDocument, elem );
	};



function adjustCSS( elem, prop, valueParts, tween ) {
	var adjusted,
		scale = 1,
		maxIterations = 20,
		currentValue = tween ?
			function() { return tween.cur(); } :
			function() { return jQuery.css( elem, prop, "" ); },
		initial = currentValue(),
		unit = valueParts && valueParts[ 3 ] || ( jQuery.cssNumber[ prop ] ? "" : "px" ),

		// Starting value computation is required for potential unit mismatches
		initialInUnit = ( jQuery.cssNumber[ prop ] || unit !== "px" && +initial ) &&
			rcssNum.exec( jQuery.css( elem, prop ) );

	if ( initialInUnit && initialInUnit[ 3 ] !== unit ) {

		// Trust units reported by jQuery.css
		unit = unit || initialInUnit[ 3 ];

		// Make sure we update the tween properties later on
		valueParts = valueParts || [];

		// Iteratively approximate from a nonzero starting point
		initialInUnit = +initial || 1;

		do {

			// If previous iteration zeroed out, double until we get *something*.
			// Use string for doubling so we don't accidentally see scale as unchanged below
			scale = scale || ".5";

			// Adjust and apply
			initialInUnit = initialInUnit / scale;
			jQuery.style( elem, prop, initialInUnit + unit );

		// Update scale, tolerating zero or NaN from tween.cur()
		// Break the loop if scale is unchanged or perfect, or if we've just had enough.
		} while (
			scale !== ( scale = currentValue() / initial ) && scale !== 1 && --maxIterations
		);
	}

	if ( valueParts ) {
		initialInUnit = +initialInUnit || +initial || 0;

		// Apply relative offset (+=/-=) if specified
		adjusted = valueParts[ 1 ] ?
			initialInUnit + ( valueParts[ 1 ] + 1 ) * valueParts[ 2 ] :
			+valueParts[ 2 ];
		if ( tween ) {
			tween.unit = unit;
			tween.start = initialInUnit;
			tween.end = adjusted;
		}
	}
	return adjusted;
}


// Multifunctional method to get and set values of a collection
// The value/s can optionally be executed if it's a function
var access = function( elems, fn, key, value, chainable, emptyGet, raw ) {
	var i = 0,
		length = elems.length,
		bulk = key == null;

	// Sets many values
	if ( jQuery.type( key ) === "object" ) {
		chainable = true;
		for ( i in key ) {
			access( elems, fn, i, key[ i ], true, emptyGet, raw );
		}

	// Sets one value
	} else if ( value !== undefined ) {
		chainable = true;

		if ( !jQuery.isFunction( value ) ) {
			raw = true;
		}

		if ( bulk ) {

			// Bulk operations run against the entire set
			if ( raw ) {
				fn.call( elems, value );
				fn = null;

			// ...except when executing function values
			} else {
				bulk = fn;
				fn = function( elem, key, value ) {
					return bulk.call( jQuery( elem ), value );
				};
			}
		}

		if ( fn ) {
			for ( ; i < length; i++ ) {
				fn(
					elems[ i ],
					key,
					raw ? value : value.call( elems[ i ], i, fn( elems[ i ], key ) )
				);
			}
		}
	}

	return chainable ?
		elems :

		// Gets
		bulk ?
			fn.call( elems ) :
			length ? fn( elems[ 0 ], key ) : emptyGet;
};
var rcheckableType = ( /^(?:checkbox|radio)$/i );

var rtagName = ( /<([\w:-]+)/ );

var rscriptType = ( /^$|\/(?:java|ecma)script/i );

var rleadingWhitespace = ( /^\s+/ );

var nodeNames = "abbr|article|aside|audio|bdi|canvas|data|datalist|" +
		"details|dialog|figcaption|figure|footer|header|hgroup|main|" +
		"mark|meter|nav|output|picture|progress|section|summary|template|time|video";



function createSafeFragment( document ) {
	var list = nodeNames.split( "|" ),
		safeFrag = document.createDocumentFragment();

	if ( safeFrag.createElement ) {
		while ( list.length ) {
			safeFrag.createElement(
				list.pop()
			);
		}
	}
	return safeFrag;
}


( function() {
	var div = document.createElement( "div" ),
		fragment = document.createDocumentFragment(),
		input = document.createElement( "input" );

	// Setup
	div.innerHTML = "  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>";

	// IE strips leading whitespace when .innerHTML is used
	support.leadingWhitespace = div.firstChild.nodeType === 3;

	// Make sure that tbody elements aren't automatically inserted
	// IE will insert them into empty tables
	support.tbody = !div.getElementsByTagName( "tbody" ).length;

	// Make sure that link elements get serialized correctly by innerHTML
	// This requires a wrapper element in IE
	support.htmlSerialize = !!div.getElementsByTagName( "link" ).length;

	// Makes sure cloning an html5 element does not cause problems
	// Where outerHTML is undefined, this still works
	support.html5Clone =
		document.createElement( "nav" ).cloneNode( true ).outerHTML !== "<:nav></:nav>";

	// Check if a disconnected checkbox will retain its checked
	// value of true after appended to the DOM (IE6/7)
	input.type = "checkbox";
	input.checked = true;
	fragment.appendChild( input );
	support.appendChecked = input.checked;

	// Make sure textarea (and checkbox) defaultValue is properly cloned
	// Support: IE6-IE11+
	div.innerHTML = "<textarea>x</textarea>";
	support.noCloneChecked = !!div.cloneNode( true ).lastChild.defaultValue;

	// #11217 - WebKit loses check when the name is after the checked attribute
	fragment.appendChild( div );

	// Support: Windows Web Apps (WWA)
	// `name` and `type` must use .setAttribute for WWA (#14901)
	input = document.createElement( "input" );
	input.setAttribute( "type", "radio" );
	input.setAttribute( "checked", "checked" );
	input.setAttribute( "name", "t" );

	div.appendChild( input );

	// Support: Safari 5.1, iOS 5.1, Android 4.x, Android 2.3
	// old WebKit doesn't clone checked state correctly in fragments
	support.checkClone = div.cloneNode( true ).cloneNode( true ).lastChild.checked;

	// Support: IE<9
	// Cloned elements keep attachEvent handlers, we use addEventListener on IE9+
	support.noCloneEvent = !!div.addEventListener;

	// Support: IE<9
	// Since attributes and properties are the same in IE,
	// cleanData must set properties to undefined rather than use removeAttribute
	div[ jQuery.expando ] = 1;
	support.attributes = !div.getAttribute( jQuery.expando );
} )();


// We have to close these tags to support XHTML (#13200)
var wrapMap = {
	option: [ 1, "<select multiple='multiple'>", "</select>" ],
	legend: [ 1, "<fieldset>", "</fieldset>" ],
	area: [ 1, "<map>", "</map>" ],

	// Support: IE8
	param: [ 1, "<object>", "</object>" ],
	thead: [ 1, "<table>", "</table>" ],
	tr: [ 2, "<table><tbody>", "</tbody></table>" ],
	col: [ 2, "<table><tbody></tbody><colgroup>", "</colgroup></table>" ],
	td: [ 3, "<table><tbody><tr>", "</tr></tbody></table>" ],

	// IE6-8 can't serialize link, script, style, or any html5 (NoScope) tags,
	// unless wrapped in a div with non-breaking characters in front of it.
	_default: support.htmlSerialize ? [ 0, "", "" ] : [ 1, "X<div>", "</div>" ]
};

// Support: IE8-IE9
wrapMap.optgroup = wrapMap.option;

wrapMap.tbody = wrapMap.tfoot = wrapMap.colgroup = wrapMap.caption = wrapMap.thead;
wrapMap.th = wrapMap.td;


function getAll( context, tag ) {
	var elems, elem,
		i = 0,
		found = typeof context.getElementsByTagName !== "undefined" ?
			context.getElementsByTagName( tag || "*" ) :
			typeof context.querySelectorAll !== "undefined" ?
				context.querySelectorAll( tag || "*" ) :
				undefined;

	if ( !found ) {
		for ( found = [], elems = context.childNodes || context;
			( elem = elems[ i ] ) != null;
			i++
		) {
			if ( !tag || jQuery.nodeName( elem, tag ) ) {
				found.push( elem );
			} else {
				jQuery.merge( found, getAll( elem, tag ) );
			}
		}
	}

	return tag === undefined || tag && jQuery.nodeName( context, tag ) ?
		jQuery.merge( [ context ], found ) :
		found;
}


// Mark scripts as having already been evaluated
function setGlobalEval( elems, refElements ) {
	var elem,
		i = 0;
	for ( ; ( elem = elems[ i ] ) != null; i++ ) {
		jQuery._data(
			elem,
			"globalEval",
			!refElements || jQuery._data( refElements[ i ], "globalEval" )
		);
	}
}


var rhtml = /<|&#?\w+;/,
	rtbody = /<tbody/i;

function fixDefaultChecked( elem ) {
	if ( rcheckableType.test( elem.type ) ) {
		elem.defaultChecked = elem.checked;
	}
}

function buildFragment( elems, context, scripts, selection, ignored ) {
	var j, elem, contains,
		tmp, tag, tbody, wrap,
		l = elems.length,

		// Ensure a safe fragment
		safe = createSafeFragment( context ),

		nodes = [],
		i = 0;

	for ( ; i < l; i++ ) {
		elem = elems[ i ];

		if ( elem || elem === 0 ) {

			// Add nodes directly
			if ( jQuery.type( elem ) === "object" ) {
				jQuery.merge( nodes, elem.nodeType ? [ elem ] : elem );

			// Convert non-html into a text node
			} else if ( !rhtml.test( elem ) ) {
				nodes.push( context.createTextNode( elem ) );

			// Convert html into DOM nodes
			} else {
				tmp = tmp || safe.appendChild( context.createElement( "div" ) );

				// Deserialize a standard representation
				tag = ( rtagName.exec( elem ) || [ "", "" ] )[ 1 ].toLowerCase();
				wrap = wrapMap[ tag ] || wrapMap._default;

				tmp.innerHTML = wrap[ 1 ] + jQuery.htmlPrefilter( elem ) + wrap[ 2 ];

				// Descend through wrappers to the right content
				j = wrap[ 0 ];
				while ( j-- ) {
					tmp = tmp.lastChild;
				}

				// Manually add leading whitespace removed by IE
				if ( !support.leadingWhitespace && rleadingWhitespace.test( elem ) ) {
					nodes.push( context.createTextNode( rleadingWhitespace.exec( elem )[ 0 ] ) );
				}

				// Remove IE's autoinserted <tbody> from table fragments
				if ( !support.tbody ) {

					// String was a <table>, *may* have spurious <tbody>
					elem = tag === "table" && !rtbody.test( elem ) ?
						tmp.firstChild :

						// String was a bare <thead> or <tfoot>
						wrap[ 1 ] === "<table>" && !rtbody.test( elem ) ?
							tmp :
							0;

					j = elem && elem.childNodes.length;
					while ( j-- ) {
						if ( jQuery.nodeName( ( tbody = elem.childNodes[ j ] ), "tbody" ) &&
							!tbody.childNodes.length ) {

							elem.removeChild( tbody );
						}
					}
				}

				jQuery.merge( nodes, tmp.childNodes );

				// Fix #12392 for WebKit and IE > 9
				tmp.textContent = "";

				// Fix #12392 for oldIE
				while ( tmp.firstChild ) {
					tmp.removeChild( tmp.firstChild );
				}

				// Remember the top-level container for proper cleanup
				tmp = safe.lastChild;
			}
		}
	}

	// Fix #11356: Clear elements from fragment
	if ( tmp ) {
		safe.removeChild( tmp );
	}

	// Reset defaultChecked for any radios and checkboxes
	// about to be appended to the DOM in IE 6/7 (#8060)
	if ( !support.appendChecked ) {
		jQuery.grep( getAll( nodes, "input" ), fixDefaultChecked );
	}

	i = 0;
	while ( ( elem = nodes[ i++ ] ) ) {

		// Skip elements already in the context collection (trac-4087)
		if ( selection && jQuery.inArray( elem, selection ) > -1 ) {
			if ( ignored ) {
				ignored.push( elem );
			}

			continue;
		}

		contains = jQuery.contains( elem.ownerDocument, elem );

		// Append to fragment
		tmp = getAll( safe.appendChild( elem ), "script" );

		// Preserve script evaluation history
		if ( contains ) {
			setGlobalEval( tmp );
		}

		// Capture executables
		if ( scripts ) {
			j = 0;
			while ( ( elem = tmp[ j++ ] ) ) {
				if ( rscriptType.test( elem.type || "" ) ) {
					scripts.push( elem );
				}
			}
		}
	}

	tmp = null;

	return safe;
}


( function() {
	var i, eventName,
		div = document.createElement( "div" );

	// Support: IE<9 (lack submit/change bubble), Firefox (lack focus(in | out) events)
	for ( i in { submit: true, change: true, focusin: true } ) {
		eventName = "on" + i;

		if ( !( support[ i ] = eventName in window ) ) {

			// Beware of CSP restrictions (https://developer.mozilla.org/en/Security/CSP)
			div.setAttribute( eventName, "t" );
			support[ i ] = div.attributes[ eventName ].expando === false;
		}
	}

	// Null elements to avoid leaks in IE.
	div = null;
} )();


var rformElems = /^(?:input|select|textarea)$/i,
	rkeyEvent = /^key/,
	rmouseEvent = /^(?:mouse|pointer|contextmenu|drag|drop)|click/,
	rfocusMorph = /^(?:focusinfocus|focusoutblur)$/,
	rtypenamespace = /^([^.]*)(?:\.(.+)|)/;

function returnTrue() {
	return true;
}

function returnFalse() {
	return false;
}

// Support: IE9
// See #13393 for more info
function safeActiveElement() {
	try {
		return document.activeElement;
	} catch ( err ) { }
}

function on( elem, types, selector, data, fn, one ) {
	var origFn, type;

	// Types can be a map of types/handlers
	if ( typeof types === "object" ) {

		// ( types-Object, selector, data )
		if ( typeof selector !== "string" ) {

			// ( types-Object, data )
			data = data || selector;
			selector = undefined;
		}
		for ( type in types ) {
			on( elem, type, selector, data, types[ type ], one );
		}
		return elem;
	}

	if ( data == null && fn == null ) {

		// ( types, fn )
		fn = selector;
		data = selector = undefined;
	} else if ( fn == null ) {
		if ( typeof selector === "string" ) {

			// ( types, selector, fn )
			fn = data;
			data = undefined;
		} else {

			// ( types, data, fn )
			fn = data;
			data = selector;
			selector = undefined;
		}
	}
	if ( fn === false ) {
		fn = returnFalse;
	} else if ( !fn ) {
		return elem;
	}

	if ( one === 1 ) {
		origFn = fn;
		fn = function( event ) {

			// Can use an empty set, since event contains the info
			jQuery().off( event );
			return origFn.apply( this, arguments );
		};

		// Use same guid so caller can remove using origFn
		fn.guid = origFn.guid || ( origFn.guid = jQuery.guid++ );
	}
	return elem.each( function() {
		jQuery.event.add( this, types, fn, data, selector );
	} );
}

/*
 * Helper functions for managing events -- not part of the public interface.
 * Props to Dean Edwards' addEvent library for many of the ideas.
 */
jQuery.event = {

	global: {},

	add: function( elem, types, handler, data, selector ) {
		var tmp, events, t, handleObjIn,
			special, eventHandle, handleObj,
			handlers, type, namespaces, origType,
			elemData = jQuery._data( elem );

		// Don't attach events to noData or text/comment nodes (but allow plain objects)
		if ( !elemData ) {
			return;
		}

		// Caller can pass in an object of custom data in lieu of the handler
		if ( handler.handler ) {
			handleObjIn = handler;
			handler = handleObjIn.handler;
			selector = handleObjIn.selector;
		}

		// Make sure that the handler has a unique ID, used to find/remove it later
		if ( !handler.guid ) {
			handler.guid = jQuery.guid++;
		}

		// Init the element's event structure and main handler, if this is the first
		if ( !( events = elemData.events ) ) {
			events = elemData.events = {};
		}
		if ( !( eventHandle = elemData.handle ) ) {
			eventHandle = elemData.handle = function( e ) {

				// Discard the second event of a jQuery.event.trigger() and
				// when an event is called after a page has unloaded
				return typeof jQuery !== "undefined" &&
					( !e || jQuery.event.triggered !== e.type ) ?
					jQuery.event.dispatch.apply( eventHandle.elem, arguments ) :
					undefined;
			};

			// Add elem as a property of the handle fn to prevent a memory leak
			// with IE non-native events
			eventHandle.elem = elem;
		}

		// Handle multiple events separated by a space
		types = ( types || "" ).match( rnotwhite ) || [ "" ];
		t = types.length;
		while ( t-- ) {
			tmp = rtypenamespace.exec( types[ t ] ) || [];
			type = origType = tmp[ 1 ];
			namespaces = ( tmp[ 2 ] || "" ).split( "." ).sort();

			// There *must* be a type, no attaching namespace-only handlers
			if ( !type ) {
				continue;
			}

			// If event changes its type, use the special event handlers for the changed type
			special = jQuery.event.special[ type ] || {};

			// If selector defined, determine special event api type, otherwise given type
			type = ( selector ? special.delegateType : special.bindType ) || type;

			// Update special based on newly reset type
			special = jQuery.event.special[ type ] || {};

			// handleObj is passed to all event handlers
			handleObj = jQuery.extend( {
				type: type,
				origType: origType,
				data: data,
				handler: handler,
				guid: handler.guid,
				selector: selector,
				needsContext: selector && jQuery.expr.match.needsContext.test( selector ),
				namespace: namespaces.join( "." )
			}, handleObjIn );

			// Init the event handler queue if we're the first
			if ( !( handlers = events[ type ] ) ) {
				handlers = events[ type ] = [];
				handlers.delegateCount = 0;

				// Only use addEventListener/attachEvent if the special events handler returns false
				if ( !special.setup ||
					special.setup.call( elem, data, namespaces, eventHandle ) === false ) {

					// Bind the global event handler to the element
					if ( elem.addEventListener ) {
						elem.addEventListener( type, eventHandle, false );

					} else if ( elem.attachEvent ) {
						elem.attachEvent( "on" + type, eventHandle );
					}
				}
			}

			if ( special.add ) {
				special.add.call( elem, handleObj );

				if ( !handleObj.handler.guid ) {
					handleObj.handler.guid = handler.guid;
				}
			}

			// Add to the element's handler list, delegates in front
			if ( selector ) {
				handlers.splice( handlers.delegateCount++, 0, handleObj );
			} else {
				handlers.push( handleObj );
			}

			// Keep track of which events have ever been used, for event optimization
			jQuery.event.global[ type ] = true;
		}

		// Nullify elem to prevent memory leaks in IE
		elem = null;
	},

	// Detach an event or set of events from an element
	remove: function( elem, types, handler, selector, mappedTypes ) {
		var j, handleObj, tmp,
			origCount, t, events,
			special, handlers, type,
			namespaces, origType,
			elemData = jQuery.hasData( elem ) && jQuery._data( elem );

		if ( !elemData || !( events = elemData.events ) ) {
			return;
		}

		// Once for each type.namespace in types; type may be omitted
		types = ( types || "" ).match( rnotwhite ) || [ "" ];
		t = types.length;
		while ( t-- ) {
			tmp = rtypenamespace.exec( types[ t ] ) || [];
			type = origType = tmp[ 1 ];
			namespaces = ( tmp[ 2 ] || "" ).split( "." ).sort();

			// Unbind all events (on this namespace, if provided) for the element
			if ( !type ) {
				for ( type in events ) {
					jQuery.event.remove( elem, type + types[ t ], handler, selector, true );
				}
				continue;
			}

			special = jQuery.event.special[ type ] || {};
			type = ( selector ? special.delegateType : special.bindType ) || type;
			handlers = events[ type ] || [];
			tmp = tmp[ 2 ] &&
				new RegExp( "(^|\\.)" + namespaces.join( "\\.(?:.*\\.|)" ) + "(\\.|$)" );

			// Remove matching events
			origCount = j = handlers.length;
			while ( j-- ) {
				handleObj = handlers[ j ];

				if ( ( mappedTypes || origType === handleObj.origType ) &&
					( !handler || handler.guid === handleObj.guid ) &&
					( !tmp || tmp.test( handleObj.namespace ) ) &&
					( !selector || selector === handleObj.selector ||
						selector === "**" && handleObj.selector ) ) {
					handlers.splice( j, 1 );

					if ( handleObj.selector ) {
						handlers.delegateCount--;
					}
					if ( special.remove ) {
						special.remove.call( elem, handleObj );
					}
				}
			}

			// Remove generic event handler if we removed something and no more handlers exist
			// (avoids potential for endless recursion during removal of special event handlers)
			if ( origCount && !handlers.length ) {
				if ( !special.teardown ||
					special.teardown.call( elem, namespaces, elemData.handle ) === false ) {

					jQuery.removeEvent( elem, type, elemData.handle );
				}

				delete events[ type ];
			}
		}

		// Remove the expando if it's no longer used
		if ( jQuery.isEmptyObject( events ) ) {
			delete elemData.handle;

			// removeData also checks for emptiness and clears the expando if empty
			// so use it instead of delete
			jQuery._removeData( elem, "events" );
		}
	},

	trigger: function( event, data, elem, onlyHandlers ) {
		var handle, ontype, cur,
			bubbleType, special, tmp, i,
			eventPath = [ elem || document ],
			type = hasOwn.call( event, "type" ) ? event.type : event,
			namespaces = hasOwn.call( event, "namespace" ) ? event.namespace.split( "." ) : [];

		cur = tmp = elem = elem || document;

		// Don't do events on text and comment nodes
		if ( elem.nodeType === 3 || elem.nodeType === 8 ) {
			return;
		}

		// focus/blur morphs to focusin/out; ensure we're not firing them right now
		if ( rfocusMorph.test( type + jQuery.event.triggered ) ) {
			return;
		}

		if ( type.indexOf( "." ) > -1 ) {

			// Namespaced trigger; create a regexp to match event type in handle()
			namespaces = type.split( "." );
			type = namespaces.shift();
			namespaces.sort();
		}
		ontype = type.indexOf( ":" ) < 0 && "on" + type;

		// Caller can pass in a jQuery.Event object, Object, or just an event type string
		event = event[ jQuery.expando ] ?
			event :
			new jQuery.Event( type, typeof event === "object" && event );

		// Trigger bitmask: & 1 for native handlers; & 2 for jQuery (always true)
		event.isTrigger = onlyHandlers ? 2 : 3;
		event.namespace = namespaces.join( "." );
		event.rnamespace = event.namespace ?
			new RegExp( "(^|\\.)" + namespaces.join( "\\.(?:.*\\.|)" ) + "(\\.|$)" ) :
			null;

		// Clean up the event in case it is being reused
		event.result = undefined;
		if ( !event.target ) {
			event.target = elem;
		}

		// Clone any incoming data and prepend the event, creating the handler arg list
		data = data == null ?
			[ event ] :
			jQuery.makeArray( data, [ event ] );

		// Allow special events to draw outside the lines
		special = jQuery.event.special[ type ] || {};
		if ( !onlyHandlers && special.trigger && special.trigger.apply( elem, data ) === false ) {
			return;
		}

		// Determine event propagation path in advance, per W3C events spec (#9951)
		// Bubble up to document, then to window; watch for a global ownerDocument var (#9724)
		if ( !onlyHandlers && !special.noBubble && !jQuery.isWindow( elem ) ) {

			bubbleType = special.delegateType || type;
			if ( !rfocusMorph.test( bubbleType + type ) ) {
				cur = cur.parentNode;
			}
			for ( ; cur; cur = cur.parentNode ) {
				eventPath.push( cur );
				tmp = cur;
			}

			// Only add window if we got to document (e.g., not plain obj or detached DOM)
			if ( tmp === ( elem.ownerDocument || document ) ) {
				eventPath.push( tmp.defaultView || tmp.parentWindow || window );
			}
		}

		// Fire handlers on the event path
		i = 0;
		while ( ( cur = eventPath[ i++ ] ) && !event.isPropagationStopped() ) {

			event.type = i > 1 ?
				bubbleType :
				special.bindType || type;

			// jQuery handler
			handle = ( jQuery._data( cur, "events" ) || {} )[ event.type ] &&
				jQuery._data( cur, "handle" );

			if ( handle ) {
				handle.apply( cur, data );
			}

			// Native handler
			handle = ontype && cur[ ontype ];
			if ( handle && handle.apply && acceptData( cur ) ) {
				event.result = handle.apply( cur, data );
				if ( event.result === false ) {
					event.preventDefault();
				}
			}
		}
		event.type = type;

		// If nobody prevented the default action, do it now
		if ( !onlyHandlers && !event.isDefaultPrevented() ) {

			if (
				( !special._default ||
				 special._default.apply( eventPath.pop(), data ) === false
				) && acceptData( elem )
			) {

				// Call a native DOM method on the target with the same name name as the event.
				// Can't use an .isFunction() check here because IE6/7 fails that test.
				// Don't do default actions on window, that's where global variables be (#6170)
				if ( ontype && elem[ type ] && !jQuery.isWindow( elem ) ) {

					// Don't re-trigger an onFOO event when we call its FOO() method
					tmp = elem[ ontype ];

					if ( tmp ) {
						elem[ ontype ] = null;
					}

					// Prevent re-triggering of the same event, since we already bubbled it above
					jQuery.event.triggered = type;
					try {
						elem[ type ]();
					} catch ( e ) {

						// IE<9 dies on focus/blur to hidden element (#1486,#12518)
						// only reproducible on winXP IE8 native, not IE9 in IE8 mode
					}
					jQuery.event.triggered = undefined;

					if ( tmp ) {
						elem[ ontype ] = tmp;
					}
				}
			}
		}

		return event.result;
	},

	dispatch: function( event ) {

		// Make a writable jQuery.Event from the native event object
		event = jQuery.event.fix( event );

		var i, j, ret, matched, handleObj,
			handlerQueue = [],
			args = slice.call( arguments ),
			handlers = ( jQuery._data( this, "events" ) || {} )[ event.type ] || [],
			special = jQuery.event.special[ event.type ] || {};

		// Use the fix-ed jQuery.Event rather than the (read-only) native event
		args[ 0 ] = event;
		event.delegateTarget = this;

		// Call the preDispatch hook for the mapped type, and let it bail if desired
		if ( special.preDispatch && special.preDispatch.call( this, event ) === false ) {
			return;
		}

		// Determine handlers
		handlerQueue = jQuery.event.handlers.call( this, event, handlers );

		// Run delegates first; they may want to stop propagation beneath us
		i = 0;
		while ( ( matched = handlerQueue[ i++ ] ) && !event.isPropagationStopped() ) {
			event.currentTarget = matched.elem;

			j = 0;
			while ( ( handleObj = matched.handlers[ j++ ] ) &&
				!event.isImmediatePropagationStopped() ) {

				// Triggered event must either 1) have no namespace, or 2) have namespace(s)
				// a subset or equal to those in the bound event (both can have no namespace).
				if ( !event.rnamespace || event.rnamespace.test( handleObj.namespace ) ) {

					event.handleObj = handleObj;
					event.data = handleObj.data;

					ret = ( ( jQuery.event.special[ handleObj.origType ] || {} ).handle ||
						handleObj.handler ).apply( matched.elem, args );

					if ( ret !== undefined ) {
						if ( ( event.result = ret ) === false ) {
							event.preventDefault();
							event.stopPropagation();
						}
					}
				}
			}
		}

		// Call the postDispatch hook for the mapped type
		if ( special.postDispatch ) {
			special.postDispatch.call( this, event );
		}

		return event.result;
	},

	handlers: function( event, handlers ) {
		var i, matches, sel, handleObj,
			handlerQueue = [],
			delegateCount = handlers.delegateCount,
			cur = event.target;

		// Support (at least): Chrome, IE9
		// Find delegate handlers
		// Black-hole SVG <use> instance trees (#13180)
		//
		// Support: Firefox<=42+
		// Avoid non-left-click in FF but don't block IE radio events (#3861, gh-2343)
		if ( delegateCount && cur.nodeType &&
			( event.type !== "click" || isNaN( event.button ) || event.button < 1 ) ) {

			/* jshint eqeqeq: false */
			for ( ; cur != this; cur = cur.parentNode || this ) {
				/* jshint eqeqeq: true */

				// Don't check non-elements (#13208)
				// Don't process clicks on disabled elements (#6911, #8165, #11382, #11764)
				if ( cur.nodeType === 1 && ( cur.disabled !== true || event.type !== "click" ) ) {
					matches = [];
					for ( i = 0; i < delegateCount; i++ ) {
						handleObj = handlers[ i ];

						// Don't conflict with Object.prototype properties (#13203)
						sel = handleObj.selector + " ";

						if ( matches[ sel ] === undefined ) {
							matches[ sel ] = handleObj.needsContext ?
								jQuery( sel, this ).index( cur ) > -1 :
								jQuery.find( sel, this, null, [ cur ] ).length;
						}
						if ( matches[ sel ] ) {
							matches.push( handleObj );
						}
					}
					if ( matches.length ) {
						handlerQueue.push( { elem: cur, handlers: matches } );
					}
				}
			}
		}

		// Add the remaining (directly-bound) handlers
		if ( delegateCount < handlers.length ) {
			handlerQueue.push( { elem: this, handlers: handlers.slice( delegateCount ) } );
		}

		return handlerQueue;
	},

	fix: function( event ) {
		if ( event[ jQuery.expando ] ) {
			return event;
		}

		// Create a writable copy of the event object and normalize some properties
		var i, prop, copy,
			type = event.type,
			originalEvent = event,
			fixHook = this.fixHooks[ type ];

		if ( !fixHook ) {
			this.fixHooks[ type ] = fixHook =
				rmouseEvent.test( type ) ? this.mouseHooks :
				rkeyEvent.test( type ) ? this.keyHooks :
				{};
		}
		copy = fixHook.props ? this.props.concat( fixHook.props ) : this.props;

		event = new jQuery.Event( originalEvent );

		i = copy.length;
		while ( i-- ) {
			prop = copy[ i ];
			event[ prop ] = originalEvent[ prop ];
		}

		// Support: IE<9
		// Fix target property (#1925)
		if ( !event.target ) {
			event.target = originalEvent.srcElement || document;
		}

		// Support: Safari 6-8+
		// Target should not be a text node (#504, #13143)
		if ( event.target.nodeType === 3 ) {
			event.target = event.target.parentNode;
		}

		// Support: IE<9
		// For mouse/key events, metaKey==false if it's undefined (#3368, #11328)
		event.metaKey = !!event.metaKey;

		return fixHook.filter ? fixHook.filter( event, originalEvent ) : event;
	},

	// Includes some event props shared by KeyEvent and MouseEvent
	props: ( "altKey bubbles cancelable ctrlKey currentTarget detail eventPhase " +
		"metaKey relatedTarget shiftKey target timeStamp view which" ).split( " " ),

	fixHooks: {},

	keyHooks: {
		props: "char charCode key keyCode".split( " " ),
		filter: function( event, original ) {

			// Add which for key events
			if ( event.which == null ) {
				event.which = original.charCode != null ? original.charCode : original.keyCode;
			}

			return event;
		}
	},

	mouseHooks: {
		props: ( "button buttons clientX clientY fromElement offsetX offsetY " +
			"pageX pageY screenX screenY toElement" ).split( " " ),
		filter: function( event, original ) {
			var body, eventDoc, doc,
				button = original.button,
				fromElement = original.fromElement;

			// Calculate pageX/Y if missing and clientX/Y available
			if ( event.pageX == null && original.clientX != null ) {
				eventDoc = event.target.ownerDocument || document;
				doc = eventDoc.documentElement;
				body = eventDoc.body;

				event.pageX = original.clientX +
					( doc && doc.scrollLeft || body && body.scrollLeft || 0 ) -
					( doc && doc.clientLeft || body && body.clientLeft || 0 );
				event.pageY = original.clientY +
					( doc && doc.scrollTop  || body && body.scrollTop  || 0 ) -
					( doc && doc.clientTop  || body && body.clientTop  || 0 );
			}

			// Add relatedTarget, if necessary
			if ( !event.relatedTarget && fromElement ) {
				event.relatedTarget = fromElement === event.target ?
					original.toElement :
					fromElement;
			}

			// Add which for click: 1 === left; 2 === middle; 3 === right
			// Note: button is not normalized, so don't use it
			if ( !event.which && button !== undefined ) {
				event.which = ( button & 1 ? 1 : ( button & 2 ? 3 : ( button & 4 ? 2 : 0 ) ) );
			}

			return event;
		}
	},

	special: {
		load: {

			// Prevent triggered image.load events from bubbling to window.load
			noBubble: true
		},
		focus: {

			// Fire native event if possible so blur/focus sequence is correct
			trigger: function() {
				if ( this !== safeActiveElement() && this.focus ) {
					try {
						this.focus();
						return false;
					} catch ( e ) {

						// Support: IE<9
						// If we error on focus to hidden element (#1486, #12518),
						// let .trigger() run the handlers
					}
				}
			},
			delegateType: "focusin"
		},
		blur: {
			trigger: function() {
				if ( this === safeActiveElement() && this.blur ) {
					this.blur();
					return false;
				}
			},
			delegateType: "focusout"
		},
		click: {

			// For checkbox, fire native event so checked state will be right
			trigger: function() {
				if ( jQuery.nodeName( this, "input" ) && this.type === "checkbox" && this.click ) {
					this.click();
					return false;
				}
			},

			// For cross-browser consistency, don't fire native .click() on links
			_default: function( event ) {
				return jQuery.nodeName( event.target, "a" );
			}
		},

		beforeunload: {
			postDispatch: function( event ) {

				// Support: Firefox 20+
				// Firefox doesn't alert if the returnValue field is not set.
				if ( event.result !== undefined && event.originalEvent ) {
					event.originalEvent.returnValue = event.result;
				}
			}
		}
	},

	// Piggyback on a donor event to simulate a different one
	simulate: function( type, elem, event ) {
		var e = jQuery.extend(
			new jQuery.Event(),
			event,
			{
				type: type,
				isSimulated: true

				// Previously, `originalEvent: {}` was set here, so stopPropagation call
				// would not be triggered on donor event, since in our own
				// jQuery.event.stopPropagation function we had a check for existence of
				// originalEvent.stopPropagation method, so, consequently it would be a noop.
				//
				// Guard for simulated events was moved to jQuery.event.stopPropagation function
				// since `originalEvent` should point to the original event for the
				// constancy with other events and for more focused logic
			}
		);

		jQuery.event.trigger( e, null, elem );

		if ( e.isDefaultPrevented() ) {
			event.preventDefault();
		}
	}
};

jQuery.removeEvent = document.removeEventListener ?
	function( elem, type, handle ) {

		// This "if" is needed for plain objects
		if ( elem.removeEventListener ) {
			elem.removeEventListener( type, handle );
		}
	} :
	function( elem, type, handle ) {
		var name = "on" + type;

		if ( elem.detachEvent ) {

			// #8545, #7054, preventing memory leaks for custom events in IE6-8
			// detachEvent needed property on element, by name of that event,
			// to properly expose it to GC
			if ( typeof elem[ name ] === "undefined" ) {
				elem[ name ] = null;
			}

			elem.detachEvent( name, handle );
		}
	};

jQuery.Event = function( src, props ) {

	// Allow instantiation without the 'new' keyword
	if ( !( this instanceof jQuery.Event ) ) {
		return new jQuery.Event( src, props );
	}

	// Event object
	if ( src && src.type ) {
		this.originalEvent = src;
		this.type = src.type;

		// Events bubbling up the document may have been marked as prevented
		// by a handler lower down the tree; reflect the correct value.
		this.isDefaultPrevented = src.defaultPrevented ||
				src.defaultPrevented === undefined &&

				// Support: IE < 9, Android < 4.0
				src.returnValue === false ?
			returnTrue :
			returnFalse;

	// Event type
	} else {
		this.type = src;
	}

	// Put explicitly provided properties onto the event object
	if ( props ) {
		jQuery.extend( this, props );
	}

	// Create a timestamp if incoming event doesn't have one
	this.timeStamp = src && src.timeStamp || jQuery.now();

	// Mark it as fixed
	this[ jQuery.expando ] = true;
};

// jQuery.Event is based on DOM3 Events as specified by the ECMAScript Language Binding
// http://www.w3.org/TR/2003/WD-DOM-Level-3-Events-20030331/ecma-script-binding.html
jQuery.Event.prototype = {
	constructor: jQuery.Event,
	isDefaultPrevented: returnFalse,
	isPropagationStopped: returnFalse,
	isImmediatePropagationStopped: returnFalse,

	preventDefault: function() {
		var e = this.originalEvent;

		this.isDefaultPrevented = returnTrue;
		if ( !e ) {
			return;
		}

		// If preventDefault exists, run it on the original event
		if ( e.preventDefault ) {
			e.preventDefault();

		// Support: IE
		// Otherwise set the returnValue property of the original event to false
		} else {
			e.returnValue = false;
		}
	},
	stopPropagation: function() {
		var e = this.originalEvent;

		this.isPropagationStopped = returnTrue;

		if ( !e || this.isSimulated ) {
			return;
		}

		// If stopPropagation exists, run it on the original event
		if ( e.stopPropagation ) {
			e.stopPropagation();
		}

		// Support: IE
		// Set the cancelBubble property of the original event to true
		e.cancelBubble = true;
	},
	stopImmediatePropagation: function() {
		var e = this.originalEvent;

		this.isImmediatePropagationStopped = returnTrue;

		if ( e && e.stopImmediatePropagation ) {
			e.stopImmediatePropagation();
		}

		this.stopPropagation();
	}
};

// Create mouseenter/leave events using mouseover/out and event-time checks
// so that event delegation works in jQuery.
// Do the same for pointerenter/pointerleave and pointerover/pointerout
//
// Support: Safari 7 only
// Safari sends mouseenter too often; see:
// https://code.google.com/p/chromium/issues/detail?id=470258
// for the description of the bug (it existed in older Chrome versions as well).
jQuery.each( {
	mouseenter: "mouseover",
	mouseleave: "mouseout",
	pointerenter: "pointerover",
	pointerleave: "pointerout"
}, function( orig, fix ) {
	jQuery.event.special[ orig ] = {
		delegateType: fix,
		bindType: fix,

		handle: function( event ) {
			var ret,
				target = this,
				related = event.relatedTarget,
				handleObj = event.handleObj;

			// For mouseenter/leave call the handler if related is outside the target.
			// NB: No relatedTarget if the mouse left/entered the browser window
			if ( !related || ( related !== target && !jQuery.contains( target, related ) ) ) {
				event.type = handleObj.origType;
				ret = handleObj.handler.apply( this, arguments );
				event.type = fix;
			}
			return ret;
		}
	};
} );

// IE submit delegation
if ( !support.submit ) {

	jQuery.event.special.submit = {
		setup: function() {

			// Only need this for delegated form submit events
			if ( jQuery.nodeName( this, "form" ) ) {
				return false;
			}

			// Lazy-add a submit handler when a descendant form may potentially be submitted
			jQuery.event.add( this, "click._submit keypress._submit", function( e ) {

				// Node name check avoids a VML-related crash in IE (#9807)
				var elem = e.target,
					form = jQuery.nodeName( elem, "input" ) || jQuery.nodeName( elem, "button" ) ?

						// Support: IE <=8
						// We use jQuery.prop instead of elem.form
						// to allow fixing the IE8 delegated submit issue (gh-2332)
						// by 3rd party polyfills/workarounds.
						jQuery.prop( elem, "form" ) :
						undefined;

				if ( form && !jQuery._data( form, "submit" ) ) {
					jQuery.event.add( form, "submit._submit", function( event ) {
						event._submitBubble = true;
					} );
					jQuery._data( form, "submit", true );
				}
			} );

			// return undefined since we don't need an event listener
		},

		postDispatch: function( event ) {

			// If form was submitted by the user, bubble the event up the tree
			if ( event._submitBubble ) {
				delete event._submitBubble;
				if ( this.parentNode && !event.isTrigger ) {
					jQuery.event.simulate( "submit", this.parentNode, event );
				}
			}
		},

		teardown: function() {

			// Only need this for delegated form submit events
			if ( jQuery.nodeName( this, "form" ) ) {
				return false;
			}

			// Remove delegated handlers; cleanData eventually reaps submit handlers attached above
			jQuery.event.remove( this, "._submit" );
		}
	};
}

// IE change delegation and checkbox/radio fix
if ( !support.change ) {

	jQuery.event.special.change = {

		setup: function() {

			if ( rformElems.test( this.nodeName ) ) {

				// IE doesn't fire change on a check/radio until blur; trigger it on click
				// after a propertychange. Eat the blur-change in special.change.handle.
				// This still fires onchange a second time for check/radio after blur.
				if ( this.type === "checkbox" || this.type === "radio" ) {
					jQuery.event.add( this, "propertychange._change", function( event ) {
						if ( event.originalEvent.propertyName === "checked" ) {
							this._justChanged = true;
						}
					} );
					jQuery.event.add( this, "click._change", function( event ) {
						if ( this._justChanged && !event.isTrigger ) {
							this._justChanged = false;
						}

						// Allow triggered, simulated change events (#11500)
						jQuery.event.simulate( "change", this, event );
					} );
				}
				return false;
			}

			// Delegated event; lazy-add a change handler on descendant inputs
			jQuery.event.add( this, "beforeactivate._change", function( e ) {
				var elem = e.target;

				if ( rformElems.test( elem.nodeName ) && !jQuery._data( elem, "change" ) ) {
					jQuery.event.add( elem, "change._change", function( event ) {
						if ( this.parentNode && !event.isSimulated && !event.isTrigger ) {
							jQuery.event.simulate( "change", this.parentNode, event );
						}
					} );
					jQuery._data( elem, "change", true );
				}
			} );
		},

		handle: function( event ) {
			var elem = event.target;

			// Swallow native change events from checkbox/radio, we already triggered them above
			if ( this !== elem || event.isSimulated || event.isTrigger ||
				( elem.type !== "radio" && elem.type !== "checkbox" ) ) {

				return event.handleObj.handler.apply( this, arguments );
			}
		},

		teardown: function() {
			jQuery.event.remove( this, "._change" );

			return !rformElems.test( this.nodeName );
		}
	};
}

// Support: Firefox
// Firefox doesn't have focus(in | out) events
// Related ticket - https://bugzilla.mozilla.org/show_bug.cgi?id=687787
//
// Support: Chrome, Safari
// focus(in | out) events fire after focus & blur events,
// which is spec violation - http://www.w3.org/TR/DOM-Level-3-Events/#events-focusevent-event-order
// Related ticket - https://code.google.com/p/chromium/issues/detail?id=449857
if ( !support.focusin ) {
	jQuery.each( { focus: "focusin", blur: "focusout" }, function( orig, fix ) {

		// Attach a single capturing handler on the document while someone wants focusin/focusout
		var handler = function( event ) {
			jQuery.event.simulate( fix, event.target, jQuery.event.fix( event ) );
		};

		jQuery.event.special[ fix ] = {
			setup: function() {
				var doc = this.ownerDocument || this,
					attaches = jQuery._data( doc, fix );

				if ( !attaches ) {
					doc.addEventListener( orig, handler, true );
				}
				jQuery._data( doc, fix, ( attaches || 0 ) + 1 );
			},
			teardown: function() {
				var doc = this.ownerDocument || this,
					attaches = jQuery._data( doc, fix ) - 1;

				if ( !attaches ) {
					doc.removeEventListener( orig, handler, true );
					jQuery._removeData( doc, fix );
				} else {
					jQuery._data( doc, fix, attaches );
				}
			}
		};
	} );
}

jQuery.fn.extend( {

	on: function( types, selector, data, fn ) {
		return on( this, types, selector, data, fn );
	},
	one: function( types, selector, data, fn ) {
		return on( this, types, selector, data, fn, 1 );
	},
	off: function( types, selector, fn ) {
		var handleObj, type;
		if ( types && types.preventDefault && types.handleObj ) {

			// ( event )  dispatched jQuery.Event
			handleObj = types.handleObj;
			jQuery( types.delegateTarget ).off(
				handleObj.namespace ?
					handleObj.origType + "." + handleObj.namespace :
					handleObj.origType,
				handleObj.selector,
				handleObj.handler
			);
			return this;
		}
		if ( typeof types === "object" ) {

			// ( types-object [, selector] )
			for ( type in types ) {
				this.off( type, selector, types[ type ] );
			}
			return this;
		}
		if ( selector === false || typeof selector === "function" ) {

			// ( types [, fn] )
			fn = selector;
			selector = undefined;
		}
		if ( fn === false ) {
			fn = returnFalse;
		}
		return this.each( function() {
			jQuery.event.remove( this, types, fn, selector );
		} );
	},

	trigger: function( type, data ) {
		return this.each( function() {
			jQuery.event.trigger( type, data, this );
		} );
	},
	triggerHandler: function( type, data ) {
		var elem = this[ 0 ];
		if ( elem ) {
			return jQuery.event.trigger( type, data, elem, true );
		}
	}
} );


var rinlinejQuery = / jQuery\d+="(?:null|\d+)"/g,
	rnoshimcache = new RegExp( "<(?:" + nodeNames + ")[\\s/>]", "i" ),
	rxhtmlTag = /<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\w:-]+)[^>]*)\/>/gi,

	// Support: IE 10-11, Edge 10240+
	// In IE/Edge using regex groups here causes severe slowdowns.
	// See https://connect.microsoft.com/IE/feedback/details/1736512/
	rnoInnerhtml = /<script|<style|<link/i,

	// checked="checked" or checked
	rchecked = /checked\s*(?:[^=]|=\s*.checked.)/i,
	rscriptTypeMasked = /^true\/(.*)/,
	rcleanScript = /^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g,
	safeFragment = createSafeFragment( document ),
	fragmentDiv = safeFragment.appendChild( document.createElement( "div" ) );

// Support: IE<8
// Manipulating tables requires a tbody
function manipulationTarget( elem, content ) {
	return jQuery.nodeName( elem, "table" ) &&
		jQuery.nodeName( content.nodeType !== 11 ? content : content.firstChild, "tr" ) ?

		elem.getElementsByTagName( "tbody" )[ 0 ] ||
			elem.appendChild( elem.ownerDocument.createElement( "tbody" ) ) :
		elem;
}

// Replace/restore the type attribute of script elements for safe DOM manipulation
function disableScript( elem ) {
	elem.type = ( jQuery.find.attr( elem, "type" ) !== null ) + "/" + elem.type;
	return elem;
}
function restoreScript( elem ) {
	var match = rscriptTypeMasked.exec( elem.type );
	if ( match ) {
		elem.type = match[ 1 ];
	} else {
		elem.removeAttribute( "type" );
	}
	return elem;
}

function cloneCopyEvent( src, dest ) {
	if ( dest.nodeType !== 1 || !jQuery.hasData( src ) ) {
		return;
	}

	var type, i, l,
		oldData = jQuery._data( src ),
		curData = jQuery._data( dest, oldData ),
		events = oldData.events;

	if ( events ) {
		delete curData.handle;
		curData.events = {};

		for ( type in events ) {
			for ( i = 0, l = events[ type ].length; i < l; i++ ) {
				jQuery.event.add( dest, type, events[ type ][ i ] );
			}
		}
	}

	// make the cloned public data object a copy from the original
	if ( curData.data ) {
		curData.data = jQuery.extend( {}, curData.data );
	}
}

function fixCloneNodeIssues( src, dest ) {
	var nodeName, e, data;

	// We do not need to do anything for non-Elements
	if ( dest.nodeType !== 1 ) {
		return;
	}

	nodeName = dest.nodeName.toLowerCase();

	// IE6-8 copies events bound via attachEvent when using cloneNode.
	if ( !support.noCloneEvent && dest[ jQuery.expando ] ) {
		data = jQuery._data( dest );

		for ( e in data.events ) {
			jQuery.removeEvent( dest, e, data.handle );
		}

		// Event data gets referenced instead of copied if the expando gets copied too
		dest.removeAttribute( jQuery.expando );
	}

	// IE blanks contents when cloning scripts, and tries to evaluate newly-set text
	if ( nodeName === "script" && dest.text !== src.text ) {
		disableScript( dest ).text = src.text;
		restoreScript( dest );

	// IE6-10 improperly clones children of object elements using classid.
	// IE10 throws NoModificationAllowedError if parent is null, #12132.
	} else if ( nodeName === "object" ) {
		if ( dest.parentNode ) {
			dest.outerHTML = src.outerHTML;
		}

		// This path appears unavoidable for IE9. When cloning an object
		// element in IE9, the outerHTML strategy above is not sufficient.
		// If the src has innerHTML and the destination does not,
		// copy the src.innerHTML into the dest.innerHTML. #10324
		if ( support.html5Clone && ( src.innerHTML && !jQuery.trim( dest.innerHTML ) ) ) {
			dest.innerHTML = src.innerHTML;
		}

	} else if ( nodeName === "input" && rcheckableType.test( src.type ) ) {

		// IE6-8 fails to persist the checked state of a cloned checkbox
		// or radio button. Worse, IE6-7 fail to give the cloned element
		// a checked appearance if the defaultChecked value isn't also set

		dest.defaultChecked = dest.checked = src.checked;

		// IE6-7 get confused and end up setting the value of a cloned
		// checkbox/radio button to an empty string instead of "on"
		if ( dest.value !== src.value ) {
			dest.value = src.value;
		}

	// IE6-8 fails to return the selected option to the default selected
	// state when cloning options
	} else if ( nodeName === "option" ) {
		dest.defaultSelected = dest.selected = src.defaultSelected;

	// IE6-8 fails to set the defaultValue to the correct value when
	// cloning other types of input fields
	} else if ( nodeName === "input" || nodeName === "textarea" ) {
		dest.defaultValue = src.defaultValue;
	}
}

function domManip( collection, args, callback, ignored ) {

	// Flatten any nested arrays
	args = concat.apply( [], args );

	var first, node, hasScripts,
		scripts, doc, fragment,
		i = 0,
		l = collection.length,
		iNoClone = l - 1,
		value = args[ 0 ],
		isFunction = jQuery.isFunction( value );

	// We can't cloneNode fragments that contain checked, in WebKit
	if ( isFunction ||
			( l > 1 && typeof value === "string" &&
				!support.checkClone && rchecked.test( value ) ) ) {
		return collection.each( function( index ) {
			var self = collection.eq( index );
			if ( isFunction ) {
				args[ 0 ] = value.call( this, index, self.html() );
			}
			domManip( self, args, callback, ignored );
		} );
	}

	if ( l ) {
		fragment = buildFragment( args, collection[ 0 ].ownerDocument, false, collection, ignored );
		first = fragment.firstChild;

		if ( fragment.childNodes.length === 1 ) {
			fragment = first;
		}

		// Require either new content or an interest in ignored elements to invoke the callback
		if ( first || ignored ) {
			scripts = jQuery.map( getAll( fragment, "script" ), disableScript );
			hasScripts = scripts.length;

			// Use the original fragment for the last item
			// instead of the first because it can end up
			// being emptied incorrectly in certain situations (#8070).
			for ( ; i < l; i++ ) {
				node = fragment;

				if ( i !== iNoClone ) {
					node = jQuery.clone( node, true, true );

					// Keep references to cloned scripts for later restoration
					if ( hasScripts ) {

						// Support: Android<4.1, PhantomJS<2
						// push.apply(_, arraylike) throws on ancient WebKit
						jQuery.merge( scripts, getAll( node, "script" ) );
					}
				}

				callback.call( collection[ i ], node, i );
			}

			if ( hasScripts ) {
				doc = scripts[ scripts.length - 1 ].ownerDocument;

				// Reenable scripts
				jQuery.map( scripts, restoreScript );

				// Evaluate executable scripts on first document insertion
				for ( i = 0; i < hasScripts; i++ ) {
					node = scripts[ i ];
					if ( rscriptType.test( node.type || "" ) &&
						!jQuery._data( node, "globalEval" ) &&
						jQuery.contains( doc, node ) ) {

						if ( node.src ) {

							// Optional AJAX dependency, but won't run scripts if not present
							if ( jQuery._evalUrl ) {
								jQuery._evalUrl( node.src );
							}
						} else {
							jQuery.globalEval(
								( node.text || node.textContent || node.innerHTML || "" )
									.replace( rcleanScript, "" )
							);
						}
					}
				}
			}

			// Fix #11809: Avoid leaking memory
			fragment = first = null;
		}
	}

	return collection;
}

function remove( elem, selector, keepData ) {
	var node,
		elems = selector ? jQuery.filter( selector, elem ) : elem,
		i = 0;

	for ( ; ( node = elems[ i ] ) != null; i++ ) {

		if ( !keepData && node.nodeType === 1 ) {
			jQuery.cleanData( getAll( node ) );
		}

		if ( node.parentNode ) {
			if ( keepData && jQuery.contains( node.ownerDocument, node ) ) {
				setGlobalEval( getAll( node, "script" ) );
			}
			node.parentNode.removeChild( node );
		}
	}

	return elem;
}

jQuery.extend( {
	htmlPrefilter: function( html ) {
		return html.replace( rxhtmlTag, "<$1></$2>" );
	},

	clone: function( elem, dataAndEvents, deepDataAndEvents ) {
		var destElements, node, clone, i, srcElements,
			inPage = jQuery.contains( elem.ownerDocument, elem );

		if ( support.html5Clone || jQuery.isXMLDoc( elem ) ||
			!rnoshimcache.test( "<" + elem.nodeName + ">" ) ) {

			clone = elem.cloneNode( true );

		// IE<=8 does not properly clone detached, unknown element nodes
		} else {
			fragmentDiv.innerHTML = elem.outerHTML;
			fragmentDiv.removeChild( clone = fragmentDiv.firstChild );
		}

		if ( ( !support.noCloneEvent || !support.noCloneChecked ) &&
				( elem.nodeType === 1 || elem.nodeType === 11 ) && !jQuery.isXMLDoc( elem ) ) {

			// We eschew Sizzle here for performance reasons: http://jsperf.com/getall-vs-sizzle/2
			destElements = getAll( clone );
			srcElements = getAll( elem );

			// Fix all IE cloning issues
			for ( i = 0; ( node = srcElements[ i ] ) != null; ++i ) {

				// Ensure that the destination node is not null; Fixes #9587
				if ( destElements[ i ] ) {
					fixCloneNodeIssues( node, destElements[ i ] );
				}
			}
		}

		// Copy the events from the original to the clone
		if ( dataAndEvents ) {
			if ( deepDataAndEvents ) {
				srcElements = srcElements || getAll( elem );
				destElements = destElements || getAll( clone );

				for ( i = 0; ( node = srcElements[ i ] ) != null; i++ ) {
					cloneCopyEvent( node, destElements[ i ] );
				}
			} else {
				cloneCopyEvent( elem, clone );
			}
		}

		// Preserve script evaluation history
		destElements = getAll( clone, "script" );
		if ( destElements.length > 0 ) {
			setGlobalEval( destElements, !inPage && getAll( elem, "script" ) );
		}

		destElements = srcElements = node = null;

		// Return the cloned set
		return clone;
	},

	cleanData: function( elems, /* internal */ forceAcceptData ) {
		var elem, type, id, data,
			i = 0,
			internalKey = jQuery.expando,
			cache = jQuery.cache,
			attributes = support.attributes,
			special = jQuery.event.special;

		for ( ; ( elem = elems[ i ] ) != null; i++ ) {
			if ( forceAcceptData || acceptData( elem ) ) {

				id = elem[ internalKey ];
				data = id && cache[ id ];

				if ( data ) {
					if ( data.events ) {
						for ( type in data.events ) {
							if ( special[ type ] ) {
								jQuery.event.remove( elem, type );

							// This is a shortcut to avoid jQuery.event.remove's overhead
							} else {
								jQuery.removeEvent( elem, type, data.handle );
							}
						}
					}

					// Remove cache only if it was not already removed by jQuery.event.remove
					if ( cache[ id ] ) {

						delete cache[ id ];

						// Support: IE<9
						// IE does not allow us to delete expando properties from nodes
						// IE creates expando attributes along with the property
						// IE does not have a removeAttribute function on Document nodes
						if ( !attributes && typeof elem.removeAttribute !== "undefined" ) {
							elem.removeAttribute( internalKey );

						// Webkit & Blink performance suffers when deleting properties
						// from DOM nodes, so set to undefined instead
						// https://code.google.com/p/chromium/issues/detail?id=378607
						} else {
							elem[ internalKey ] = undefined;
						}

						deletedIds.push( id );
					}
				}
			}
		}
	}
} );

jQuery.fn.extend( {

	// Keep domManip exposed until 3.0 (gh-2225)
	domManip: domManip,

	detach: function( selector ) {
		return remove( this, selector, true );
	},

	remove: function( selector ) {
		return remove( this, selector );
	},

	text: function( value ) {
		return access( this, function( value ) {
			return value === undefined ?
				jQuery.text( this ) :
				this.empty().append(
					( this[ 0 ] && this[ 0 ].ownerDocument || document ).createTextNode( value )
				);
		}, null, value, arguments.length );
	},

	append: function() {
		return domManip( this, arguments, function( elem ) {
			if ( this.nodeType === 1 || this.nodeType === 11 || this.nodeType === 9 ) {
				var target = manipulationTarget( this, elem );
				target.appendChild( elem );
			}
		} );
	},

	prepend: function() {
		return domManip( this, arguments, function( elem ) {
			if ( this.nodeType === 1 || this.nodeType === 11 || this.nodeType === 9 ) {
				var target = manipulationTarget( this, elem );
				target.insertBefore( elem, target.firstChild );
			}
		} );
	},

	before: function() {
		return domManip( this, arguments, function( elem ) {
			if ( this.parentNode ) {
				this.parentNode.insertBefore( elem, this );
			}
		} );
	},

	after: function() {
		return domManip( this, arguments, function( elem ) {
			if ( this.parentNode ) {
				this.parentNode.insertBefore( elem, this.nextSibling );
			}
		} );
	},

	empty: function() {
		var elem,
			i = 0;

		for ( ; ( elem = this[ i ] ) != null; i++ ) {

			// Remove element nodes and prevent memory leaks
			if ( elem.nodeType === 1 ) {
				jQuery.cleanData( getAll( elem, false ) );
			}

			// Remove any remaining nodes
			while ( elem.firstChild ) {
				elem.removeChild( elem.firstChild );
			}

			// If this is a select, ensure that it displays empty (#12336)
			// Support: IE<9
			if ( elem.options && jQuery.nodeName( elem, "select" ) ) {
				elem.options.length = 0;
			}
		}

		return this;
	},

	clone: function( dataAndEvents, deepDataAndEvents ) {
		dataAndEvents = dataAndEvents == null ? false : dataAndEvents;
		deepDataAndEvents = deepDataAndEvents == null ? dataAndEvents : deepDataAndEvents;

		return this.map( function() {
			return jQuery.clone( this, dataAndEvents, deepDataAndEvents );
		} );
	},

	html: function( value ) {
		return access( this, function( value ) {
			var elem = this[ 0 ] || {},
				i = 0,
				l = this.length;

			if ( value === undefined ) {
				return elem.nodeType === 1 ?
					elem.innerHTML.replace( rinlinejQuery, "" ) :
					undefined;
			}

			// See if we can take a shortcut and just use innerHTML
			if ( typeof value === "string" && !rnoInnerhtml.test( value ) &&
				( support.htmlSerialize || !rnoshimcache.test( value )  ) &&
				( support.leadingWhitespace || !rleadingWhitespace.test( value ) ) &&
				!wrapMap[ ( rtagName.exec( value ) || [ "", "" ] )[ 1 ].toLowerCase() ] ) {

				value = jQuery.htmlPrefilter( value );

				try {
					for ( ; i < l; i++ ) {

						// Remove element nodes and prevent memory leaks
						elem = this[ i ] || {};
						if ( elem.nodeType === 1 ) {
							jQuery.cleanData( getAll( elem, false ) );
							elem.innerHTML = value;
						}
					}

					elem = 0;

				// If using innerHTML throws an exception, use the fallback method
				} catch ( e ) {}
			}

			if ( elem ) {
				this.empty().append( value );
			}
		}, null, value, arguments.length );
	},

	replaceWith: function() {
		var ignored = [];

		// Make the changes, replacing each non-ignored context element with the new content
		return domManip( this, arguments, function( elem ) {
			var parent = this.parentNode;

			if ( jQuery.inArray( this, ignored ) < 0 ) {
				jQuery.cleanData( getAll( this ) );
				if ( parent ) {
					parent.replaceChild( elem, this );
				}
			}

		// Force callback invocation
		}, ignored );
	}
} );

jQuery.each( {
	appendTo: "append",
	prependTo: "prepend",
	insertBefore: "before",
	insertAfter: "after",
	replaceAll: "replaceWith"
}, function( name, original ) {
	jQuery.fn[ name ] = function( selector ) {
		var elems,
			i = 0,
			ret = [],
			insert = jQuery( selector ),
			last = insert.length - 1;

		for ( ; i <= last; i++ ) {
			elems = i === last ? this : this.clone( true );
			jQuery( insert[ i ] )[ original ]( elems );

			// Modern browsers can apply jQuery collections as arrays, but oldIE needs a .get()
			push.apply( ret, elems.get() );
		}

		return this.pushStack( ret );
	};
} );


var iframe,
	elemdisplay = {

		// Support: Firefox
		// We have to pre-define these values for FF (#10227)
		HTML: "block",
		BODY: "block"
	};

/**
 * Retrieve the actual display of a element
 * @param {String} name nodeName of the element
 * @param {Object} doc Document object
 */

// Called only from within defaultDisplay
function actualDisplay( name, doc ) {
	var elem = jQuery( doc.createElement( name ) ).appendTo( doc.body ),

		display = jQuery.css( elem[ 0 ], "display" );

	// We don't have any data stored on the element,
	// so use "detach" method as fast way to get rid of the element
	elem.detach();

	return display;
}

/**
 * Try to determine the default display value of an element
 * @param {String} nodeName
 */
function defaultDisplay( nodeName ) {
	var doc = document,
		display = elemdisplay[ nodeName ];

	if ( !display ) {
		display = actualDisplay( nodeName, doc );

		// If the simple way fails, read from inside an iframe
		if ( display === "none" || !display ) {

			// Use the already-created iframe if possible
			iframe = ( iframe || jQuery( "<iframe frameborder='0' width='0' height='0'/>" ) )
				.appendTo( doc.documentElement );

			// Always write a new HTML skeleton so Webkit and Firefox don't choke on reuse
			doc = ( iframe[ 0 ].contentWindow || iframe[ 0 ].contentDocument ).document;

			// Support: IE
			doc.write();
			doc.close();

			display = actualDisplay( nodeName, doc );
			iframe.detach();
		}

		// Store the correct default display
		elemdisplay[ nodeName ] = display;
	}

	return display;
}
var rmargin = ( /^margin/ );

var rnumnonpx = new RegExp( "^(" + pnum + ")(?!px)[a-z%]+$", "i" );

var swap = function( elem, options, callback, args ) {
	var ret, name,
		old = {};

	// Remember the old values, and insert the new ones
	for ( name in options ) {
		old[ name ] = elem.style[ name ];
		elem.style[ name ] = options[ name ];
	}

	ret = callback.apply( elem, args || [] );

	// Revert the old values
	for ( name in options ) {
		elem.style[ name ] = old[ name ];
	}

	return ret;
};


var documentElement = document.documentElement;



( function() {
	var pixelPositionVal, pixelMarginRightVal, boxSizingReliableVal,
		reliableHiddenOffsetsVal, reliableMarginRightVal, reliableMarginLeftVal,
		container = document.createElement( "div" ),
		div = document.createElement( "div" );

	// Finish early in limited (non-browser) environments
	if ( !div.style ) {
		return;
	}

	div.style.cssText = "float:left;opacity:.5";

	// Support: IE<9
	// Make sure that element opacity exists (as opposed to filter)
	support.opacity = div.style.opacity === "0.5";

	// Verify style float existence
	// (IE uses styleFloat instead of cssFloat)
	support.cssFloat = !!div.style.cssFloat;

	div.style.backgroundClip = "content-box";
	div.cloneNode( true ).style.backgroundClip = "";
	support.clearCloneStyle = div.style.backgroundClip === "content-box";

	container = document.createElement( "div" );
	container.style.cssText = "border:0;width:8px;height:0;top:0;left:-9999px;" +
		"padding:0;margin-top:1px;position:absolute";
	div.innerHTML = "";
	container.appendChild( div );

	// Support: Firefox<29, Android 2.3
	// Vendor-prefix box-sizing
	support.boxSizing = div.style.boxSizing === "" || div.style.MozBoxSizing === "" ||
		div.style.WebkitBoxSizing === "";

	jQuery.extend( support, {
		reliableHiddenOffsets: function() {
			if ( pixelPositionVal == null ) {
				computeStyleTests();
			}
			return reliableHiddenOffsetsVal;
		},

		boxSizingReliable: function() {

			// We're checking for pixelPositionVal here instead of boxSizingReliableVal
			// since that compresses better and they're computed together anyway.
			if ( pixelPositionVal == null ) {
				computeStyleTests();
			}
			return boxSizingReliableVal;
		},

		pixelMarginRight: function() {

			// Support: Android 4.0-4.3
			if ( pixelPositionVal == null ) {
				computeStyleTests();
			}
			return pixelMarginRightVal;
		},

		pixelPosition: function() {
			if ( pixelPositionVal == null ) {
				computeStyleTests();
			}
			return pixelPositionVal;
		},

		reliableMarginRight: function() {

			// Support: Android 2.3
			if ( pixelPositionVal == null ) {
				computeStyleTests();
			}
			return reliableMarginRightVal;
		},

		reliableMarginLeft: function() {

			// Support: IE <=8 only, Android 4.0 - 4.3 only, Firefox <=3 - 37
			if ( pixelPositionVal == null ) {
				computeStyleTests();
			}
			return reliableMarginLeftVal;
		}
	} );

	function computeStyleTests() {
		var contents, divStyle,
			documentElement = document.documentElement;

		// Setup
		documentElement.appendChild( container );

		div.style.cssText =

			// Support: Android 2.3
			// Vendor-prefix box-sizing
			"-webkit-box-sizing:border-box;box-sizing:border-box;" +
			"position:relative;display:block;" +
			"margin:auto;border:1px;padding:1px;" +
			"top:1%;width:50%";

		// Support: IE<9
		// Assume reasonable values in the absence of getComputedStyle
		pixelPositionVal = boxSizingReliableVal = reliableMarginLeftVal = false;
		pixelMarginRightVal = reliableMarginRightVal = true;

		// Check for getComputedStyle so that this code is not run in IE<9.
		if ( window.getComputedStyle ) {
			divStyle = window.getComputedStyle( div );
			pixelPositionVal = ( divStyle || {} ).top !== "1%";
			reliableMarginLeftVal = ( divStyle || {} ).marginLeft === "2px";
			boxSizingReliableVal = ( divStyle || { width: "4px" } ).width === "4px";

			// Support: Android 4.0 - 4.3 only
			// Some styles come back with percentage values, even though they shouldn't
			div.style.marginRight = "50%";
			pixelMarginRightVal = ( divStyle || { marginRight: "4px" } ).marginRight === "4px";

			// Support: Android 2.3 only
			// Div with explicit width and no margin-right incorrectly
			// gets computed margin-right based on width of container (#3333)
			// WebKit Bug 13343 - getComputedStyle returns wrong value for margin-right
			contents = div.appendChild( document.createElement( "div" ) );

			// Reset CSS: box-sizing; display; margin; border; padding
			contents.style.cssText = div.style.cssText =

				// Support: Android 2.3
				// Vendor-prefix box-sizing
				"-webkit-box-sizing:content-box;-moz-box-sizing:content-box;" +
				"box-sizing:content-box;display:block;margin:0;border:0;padding:0";
			contents.style.marginRight = contents.style.width = "0";
			div.style.width = "1px";

			reliableMarginRightVal =
				!parseFloat( ( window.getComputedStyle( contents ) || {} ).marginRight );

			div.removeChild( contents );
		}

		// Support: IE6-8
		// First check that getClientRects works as expected
		// Check if table cells still have offsetWidth/Height when they are set
		// to display:none and there are still other visible table cells in a
		// table row; if so, offsetWidth/Height are not reliable for use when
		// determining if an element has been hidden directly using
		// display:none (it is still safe to use offsets if a parent element is
		// hidden; don safety goggles and see bug #4512 for more information).
		div.style.display = "none";
		reliableHiddenOffsetsVal = div.getClientRects().length === 0;
		if ( reliableHiddenOffsetsVal ) {
			div.style.display = "";
			div.innerHTML = "<table><tr><td></td><td>t</td></tr></table>";
			contents = div.getElementsByTagName( "td" );
			contents[ 0 ].style.cssText = "margin:0;border:0;padding:0;display:none";
			reliableHiddenOffsetsVal = contents[ 0 ].offsetHeight === 0;
			if ( reliableHiddenOffsetsVal ) {
				contents[ 0 ].style.display = "";
				contents[ 1 ].style.display = "none";
				reliableHiddenOffsetsVal = contents[ 0 ].offsetHeight === 0;
			}
		}

		// Teardown
		documentElement.removeChild( container );
	}

} )();


var getStyles, curCSS,
	rposition = /^(top|right|bottom|left)$/;

if ( window.getComputedStyle ) {
	getStyles = function( elem ) {

		// Support: IE<=11+, Firefox<=30+ (#15098, #14150)
		// IE throws on elements created in popups
		// FF meanwhile throws on frame elements through "defaultView.getComputedStyle"
		var view = elem.ownerDocument.defaultView;

		if ( !view.opener ) {
			view = window;
		}

		return view.getComputedStyle( elem );
	};

	curCSS = function( elem, name, computed ) {
		var width, minWidth, maxWidth, ret,
			style = elem.style;

		computed = computed || getStyles( elem );

		// getPropertyValue is only needed for .css('filter') in IE9, see #12537
		ret = computed ? computed.getPropertyValue( name ) || computed[ name ] : undefined;

		if ( computed ) {

			if ( ret === "" && !jQuery.contains( elem.ownerDocument, elem ) ) {
				ret = jQuery.style( elem, name );
			}

			// A tribute to the "awesome hack by Dean Edwards"
			// Chrome < 17 and Safari 5.0 uses "computed value"
			// instead of "used value" for margin-right
			// Safari 5.1.7 (at least) returns percentage for a larger set of values,
			// but width seems to be reliably pixels
			// this is against the CSSOM draft spec:
			// http://dev.w3.org/csswg/cssom/#resolved-values
			if ( !support.pixelMarginRight() && rnumnonpx.test( ret ) && rmargin.test( name ) ) {

				// Remember the original values
				width = style.width;
				minWidth = style.minWidth;
				maxWidth = style.maxWidth;

				// Put in the new values to get a computed value out
				style.minWidth = style.maxWidth = style.width = ret;
				ret = computed.width;

				// Revert the changed values
				style.width = width;
				style.minWidth = minWidth;
				style.maxWidth = maxWidth;
			}
		}

		// Support: IE
		// IE returns zIndex value as an integer.
		return ret === undefined ?
			ret :
			ret + "";
	};
} else if ( documentElement.currentStyle ) {
	getStyles = function( elem ) {
		return elem.currentStyle;
	};

	curCSS = function( elem, name, computed ) {
		var left, rs, rsLeft, ret,
			style = elem.style;

		computed = computed || getStyles( elem );
		ret = computed ? computed[ name ] : undefined;

		// Avoid setting ret to empty string here
		// so we don't default to auto
		if ( ret == null && style && style[ name ] ) {
			ret = style[ name ];
		}

		// From the awesome hack by Dean Edwards
		// http://erik.eae.net/archives/2007/07/27/18.54.15/#comment-102291

		// If we're not dealing with a regular pixel number
		// but a number that has a weird ending, we need to convert it to pixels
		// but not position css attributes, as those are
		// proportional to the parent element instead
		// and we can't measure the parent instead because it
		// might trigger a "stacking dolls" problem
		if ( rnumnonpx.test( ret ) && !rposition.test( name ) ) {

			// Remember the original values
			left = style.left;
			rs = elem.runtimeStyle;
			rsLeft = rs && rs.left;

			// Put in the new values to get a computed value out
			if ( rsLeft ) {
				rs.left = elem.currentStyle.left;
			}
			style.left = name === "fontSize" ? "1em" : ret;
			ret = style.pixelLeft + "px";

			// Revert the changed values
			style.left = left;
			if ( rsLeft ) {
				rs.left = rsLeft;
			}
		}

		// Support: IE
		// IE returns zIndex value as an integer.
		return ret === undefined ?
			ret :
			ret + "" || "auto";
	};
}




function addGetHookIf( conditionFn, hookFn ) {

	// Define the hook, we'll check on the first run if it's really needed.
	return {
		get: function() {
			if ( conditionFn() ) {

				// Hook not needed (or it's not possible to use it due
				// to missing dependency), remove it.
				delete this.get;
				return;
			}

			// Hook needed; redefine it so that the support test is not executed again.
			return ( this.get = hookFn ).apply( this, arguments );
		}
	};
}


var

		ralpha = /alpha\([^)]*\)/i,
	ropacity = /opacity\s*=\s*([^)]*)/i,

	// swappable if display is none or starts with table except
	// "table", "table-cell", or "table-caption"
	// see here for display values:
	// https://developer.mozilla.org/en-US/docs/CSS/display
	rdisplayswap = /^(none|table(?!-c[ea]).+)/,
	rnumsplit = new RegExp( "^(" + pnum + ")(.*)$", "i" ),

	cssShow = { position: "absolute", visibility: "hidden", display: "block" },
	cssNormalTransform = {
		letterSpacing: "0",
		fontWeight: "400"
	},

	cssPrefixes = [ "Webkit", "O", "Moz", "ms" ],
	emptyStyle = document.createElement( "div" ).style;


// return a css property mapped to a potentially vendor prefixed property
function vendorPropName( name ) {

	// shortcut for names that are not vendor prefixed
	if ( name in emptyStyle ) {
		return name;
	}

	// check for vendor prefixed names
	var capName = name.charAt( 0 ).toUpperCase() + name.slice( 1 ),
		i = cssPrefixes.length;

	while ( i-- ) {
		name = cssPrefixes[ i ] + capName;
		if ( name in emptyStyle ) {
			return name;
		}
	}
}

function showHide( elements, show ) {
	var display, elem, hidden,
		values = [],
		index = 0,
		length = elements.length;

	for ( ; index < length; index++ ) {
		elem = elements[ index ];
		if ( !elem.style ) {
			continue;
		}

		values[ index ] = jQuery._data( elem, "olddisplay" );
		display = elem.style.display;
		if ( show ) {

			// Reset the inline display of this element to learn if it is
			// being hidden by cascaded rules or not
			if ( !values[ index ] && display === "none" ) {
				elem.style.display = "";
			}

			// Set elements which have been overridden with display: none
			// in a stylesheet to whatever the default browser style is
			// for such an element
			if ( elem.style.display === "" && isHidden( elem ) ) {
				values[ index ] =
					jQuery._data( elem, "olddisplay", defaultDisplay( elem.nodeName ) );
			}
		} else {
			hidden = isHidden( elem );

			if ( display && display !== "none" || !hidden ) {
				jQuery._data(
					elem,
					"olddisplay",
					hidden ? display : jQuery.css( elem, "display" )
				);
			}
		}
	}

	// Set the display of most of the elements in a second loop
	// to avoid the constant reflow
	for ( index = 0; index < length; index++ ) {
		elem = elements[ index ];
		if ( !elem.style ) {
			continue;
		}
		if ( !show || elem.style.display === "none" || elem.style.display === "" ) {
			elem.style.display = show ? values[ index ] || "" : "none";
		}
	}

	return elements;
}

function setPositiveNumber( elem, value, subtract ) {
	var matches = rnumsplit.exec( value );
	return matches ?

		// Guard against undefined "subtract", e.g., when used as in cssHooks
		Math.max( 0, matches[ 1 ] - ( subtract || 0 ) ) + ( matches[ 2 ] || "px" ) :
		value;
}

function augmentWidthOrHeight( elem, name, extra, isBorderBox, styles ) {
	var i = extra === ( isBorderBox ? "border" : "content" ) ?

		// If we already have the right measurement, avoid augmentation
		4 :

		// Otherwise initialize for horizontal or vertical properties
		name === "width" ? 1 : 0,

		val = 0;

	for ( ; i < 4; i += 2 ) {

		// both box models exclude margin, so add it if we want it
		if ( extra === "margin" ) {
			val += jQuery.css( elem, extra + cssExpand[ i ], true, styles );
		}

		if ( isBorderBox ) {

			// border-box includes padding, so remove it if we want content
			if ( extra === "content" ) {
				val -= jQuery.css( elem, "padding" + cssExpand[ i ], true, styles );
			}

			// at this point, extra isn't border nor margin, so remove border
			if ( extra !== "margin" ) {
				val -= jQuery.css( elem, "border" + cssExpand[ i ] + "Width", true, styles );
			}
		} else {

			// at this point, extra isn't content, so add padding
			val += jQuery.css( elem, "padding" + cssExpand[ i ], true, styles );

			// at this point, extra isn't content nor padding, so add border
			if ( extra !== "padding" ) {
				val += jQuery.css( elem, "border" + cssExpand[ i ] + "Width", true, styles );
			}
		}
	}

	return val;
}

function getWidthOrHeight( elem, name, extra ) {

	// Start with offset property, which is equivalent to the border-box value
	var valueIsBorderBox = true,
		val = name === "width" ? elem.offsetWidth : elem.offsetHeight,
		styles = getStyles( elem ),
		isBorderBox = support.boxSizing &&
			jQuery.css( elem, "boxSizing", false, styles ) === "border-box";

	// Support: IE11 only
	// In IE 11 fullscreen elements inside of an iframe have
	// 100x too small dimensions (gh-1764).
	if ( document.msFullscreenElement && window.top !== window ) {

		// Support: IE11 only
		// Running getBoundingClientRect on a disconnected node
		// in IE throws an error.
		if ( elem.getClientRects().length ) {
			val = Math.round( elem.getBoundingClientRect()[ name ] * 100 );
		}
	}

	// some non-html elements return undefined for offsetWidth, so check for null/undefined
	// svg - https://bugzilla.mozilla.org/show_bug.cgi?id=649285
	// MathML - https://bugzilla.mozilla.org/show_bug.cgi?id=491668
	if ( val <= 0 || val == null ) {

		// Fall back to computed then uncomputed css if necessary
		val = curCSS( elem, name, styles );
		if ( val < 0 || val == null ) {
			val = elem.style[ name ];
		}

		// Computed unit is not pixels. Stop here and return.
		if ( rnumnonpx.test( val ) ) {
			return val;
		}

		// we need the check for style in case a browser which returns unreliable values
		// for getComputedStyle silently falls back to the reliable elem.style
		valueIsBorderBox = isBorderBox &&
			( support.boxSizingReliable() || val === elem.style[ name ] );

		// Normalize "", auto, and prepare for extra
		val = parseFloat( val ) || 0;
	}

	// use the active box-sizing model to add/subtract irrelevant styles
	return ( val +
		augmentWidthOrHeight(
			elem,
			name,
			extra || ( isBorderBox ? "border" : "content" ),
			valueIsBorderBox,
			styles
		)
	) + "px";
}

jQuery.extend( {

	// Add in style property hooks for overriding the default
	// behavior of getting and setting a style property
	cssHooks: {
		opacity: {
			get: function( elem, computed ) {
				if ( computed ) {

					// We should always get a number back from opacity
					var ret = curCSS( elem, "opacity" );
					return ret === "" ? "1" : ret;
				}
			}
		}
	},

	// Don't automatically add "px" to these possibly-unitless properties
	cssNumber: {
		"animationIterationCount": true,
		"columnCount": true,
		"fillOpacity": true,
		"flexGrow": true,
		"flexShrink": true,
		"fontWeight": true,
		"lineHeight": true,
		"opacity": true,
		"order": true,
		"orphans": true,
		"widows": true,
		"zIndex": true,
		"zoom": true
	},

	// Add in properties whose names you wish to fix before
	// setting or getting the value
	cssProps: {

		// normalize float css property
		"float": support.cssFloat ? "cssFloat" : "styleFloat"
	},

	// Get and set the style property on a DOM Node
	style: function( elem, name, value, extra ) {

		// Don't set styles on text and comment nodes
		if ( !elem || elem.nodeType === 3 || elem.nodeType === 8 || !elem.style ) {
			return;
		}

		// Make sure that we're working with the right name
		var ret, type, hooks,
			origName = jQuery.camelCase( name ),
			style = elem.style;

		name = jQuery.cssProps[ origName ] ||
			( jQuery.cssProps[ origName ] = vendorPropName( origName ) || origName );

		// gets hook for the prefixed version
		// followed by the unprefixed version
		hooks = jQuery.cssHooks[ name ] || jQuery.cssHooks[ origName ];

		// Check if we're setting a value
		if ( value !== undefined ) {
			type = typeof value;

			// Convert "+=" or "-=" to relative numbers (#7345)
			if ( type === "string" && ( ret = rcssNum.exec( value ) ) && ret[ 1 ] ) {
				value = adjustCSS( elem, name, ret );

				// Fixes bug #9237
				type = "number";
			}

			// Make sure that null and NaN values aren't set. See: #7116
			if ( value == null || value !== value ) {
				return;
			}

			// If a number was passed in, add the unit (except for certain CSS properties)
			if ( type === "number" ) {
				value += ret && ret[ 3 ] || ( jQuery.cssNumber[ origName ] ? "" : "px" );
			}

			// Fixes #8908, it can be done more correctly by specifing setters in cssHooks,
			// but it would mean to define eight
			// (for every problematic property) identical functions
			if ( !support.clearCloneStyle && value === "" && name.indexOf( "background" ) === 0 ) {
				style[ name ] = "inherit";
			}

			// If a hook was provided, use that value, otherwise just set the specified value
			if ( !hooks || !( "set" in hooks ) ||
				( value = hooks.set( elem, value, extra ) ) !== undefined ) {

				// Support: IE
				// Swallow errors from 'invalid' CSS values (#5509)
				try {
					style[ name ] = value;
				} catch ( e ) {}
			}

		} else {

			// If a hook was provided get the non-computed value from there
			if ( hooks && "get" in hooks &&
				( ret = hooks.get( elem, false, extra ) ) !== undefined ) {

				return ret;
			}

			// Otherwise just get the value from the style object
			return style[ name ];
		}
	},

	css: function( elem, name, extra, styles ) {
		var num, val, hooks,
			origName = jQuery.camelCase( name );

		// Make sure that we're working with the right name
		name = jQuery.cssProps[ origName ] ||
			( jQuery.cssProps[ origName ] = vendorPropName( origName ) || origName );

		// gets hook for the prefixed version
		// followed by the unprefixed version
		hooks = jQuery.cssHooks[ name ] || jQuery.cssHooks[ origName ];

		// If a hook was provided get the computed value from there
		if ( hooks && "get" in hooks ) {
			val = hooks.get( elem, true, extra );
		}

		// Otherwise, if a way to get the computed value exists, use that
		if ( val === undefined ) {
			val = curCSS( elem, name, styles );
		}

		//convert "normal" to computed value
		if ( val === "normal" && name in cssNormalTransform ) {
			val = cssNormalTransform[ name ];
		}

		// Return, converting to number if forced or a qualifier was provided and val looks numeric
		if ( extra === "" || extra ) {
			num = parseFloat( val );
			return extra === true || isFinite( num ) ? num || 0 : val;
		}
		return val;
	}
} );

jQuery.each( [ "height", "width" ], function( i, name ) {
	jQuery.cssHooks[ name ] = {
		get: function( elem, computed, extra ) {
			if ( computed ) {

				// certain elements can have dimension info if we invisibly show them
				// however, it must have a current display style that would benefit from this
				return rdisplayswap.test( jQuery.css( elem, "display" ) ) &&
					elem.offsetWidth === 0 ?
						swap( elem, cssShow, function() {
							return getWidthOrHeight( elem, name, extra );
						} ) :
						getWidthOrHeight( elem, name, extra );
			}
		},

		set: function( elem, value, extra ) {
			var styles = extra && getStyles( elem );
			return setPositiveNumber( elem, value, extra ?
				augmentWidthOrHeight(
					elem,
					name,
					extra,
					support.boxSizing &&
						jQuery.css( elem, "boxSizing", false, styles ) === "border-box",
					styles
				) : 0
			);
		}
	};
} );

if ( !support.opacity ) {
	jQuery.cssHooks.opacity = {
		get: function( elem, computed ) {

			// IE uses filters for opacity
			return ropacity.test( ( computed && elem.currentStyle ?
				elem.currentStyle.filter :
				elem.style.filter ) || "" ) ?
					( 0.01 * parseFloat( RegExp.$1 ) ) + "" :
					computed ? "1" : "";
		},

		set: function( elem, value ) {
			var style = elem.style,
				currentStyle = elem.currentStyle,
				opacity = jQuery.isNumeric( value ) ? "alpha(opacity=" + value * 100 + ")" : "",
				filter = currentStyle && currentStyle.filter || style.filter || "";

			// IE has trouble with opacity if it does not have layout
			// Force it by setting the zoom level
			style.zoom = 1;

			// if setting opacity to 1, and no other filters exist -
			// attempt to remove filter attribute #6652
			// if value === "", then remove inline opacity #12685
			if ( ( value >= 1 || value === "" ) &&
					jQuery.trim( filter.replace( ralpha, "" ) ) === "" &&
					style.removeAttribute ) {

				// Setting style.filter to null, "" & " " still leave "filter:" in the cssText
				// if "filter:" is present at all, clearType is disabled, we want to avoid this
				// style.removeAttribute is IE Only, but so apparently is this code path...
				style.removeAttribute( "filter" );

				// if there is no filter style applied in a css rule
				// or unset inline opacity, we are done
				if ( value === "" || currentStyle && !currentStyle.filter ) {
					return;
				}
			}

			// otherwise, set new filter values
			style.filter = ralpha.test( filter ) ?
				filter.replace( ralpha, opacity ) :
				filter + " " + opacity;
		}
	};
}

jQuery.cssHooks.marginRight = addGetHookIf( support.reliableMarginRight,
	function( elem, computed ) {
		if ( computed ) {
			return swap( elem, { "display": "inline-block" },
				curCSS, [ elem, "marginRight" ] );
		}
	}
);

jQuery.cssHooks.marginLeft = addGetHookIf( support.reliableMarginLeft,
	function( elem, computed ) {
		if ( computed ) {
			return (
				parseFloat( curCSS( elem, "marginLeft" ) ) ||

				// Support: IE<=11+
				// Running getBoundingClientRect on a disconnected node in IE throws an error
				// Support: IE8 only
				// getClientRects() errors on disconnected elems
				( jQuery.contains( elem.ownerDocument, elem ) ?
					elem.getBoundingClientRect().left -
						swap( elem, { marginLeft: 0 }, function() {
							return elem.getBoundingClientRect().left;
						} ) :
					0
				)
			) + "px";
		}
	}
);

// These hooks are used by animate to expand properties
jQuery.each( {
	margin: "",
	padding: "",
	border: "Width"
}, function( prefix, suffix ) {
	jQuery.cssHooks[ prefix + suffix ] = {
		expand: function( value ) {
			var i = 0,
				expanded = {},

				// assumes a single number if not a string
				parts = typeof value === "string" ? value.split( " " ) : [ value ];

			for ( ; i < 4; i++ ) {
				expanded[ prefix + cssExpand[ i ] + suffix ] =
					parts[ i ] || parts[ i - 2 ] || parts[ 0 ];
			}

			return expanded;
		}
	};

	if ( !rmargin.test( prefix ) ) {
		jQuery.cssHooks[ prefix + suffix ].set = setPositiveNumber;
	}
} );

jQuery.fn.extend( {
	css: function( name, value ) {
		return access( this, function( elem, name, value ) {
			var styles, len,
				map = {},
				i = 0;

			if ( jQuery.isArray( name ) ) {
				styles = getStyles( elem );
				len = name.length;

				for ( ; i < len; i++ ) {
					map[ name[ i ] ] = jQuery.css( elem, name[ i ], false, styles );
				}

				return map;
			}

			return value !== undefined ?
				jQuery.style( elem, name, value ) :
				jQuery.css( elem, name );
		}, name, value, arguments.length > 1 );
	},
	show: function() {
		return showHide( this, true );
	},
	hide: function() {
		return showHide( this );
	},
	toggle: function( state ) {
		if ( typeof state === "boolean" ) {
			return state ? this.show() : this.hide();
		}

		return this.each( function() {
			if ( isHidden( this ) ) {
				jQuery( this ).show();
			} else {
				jQuery( this ).hide();
			}
		} );
	}
} );


function Tween( elem, options, prop, end, easing ) {
	return new Tween.prototype.init( elem, options, prop, end, easing );
}
jQuery.Tween = Tween;

Tween.prototype = {
	constructor: Tween,
	init: function( elem, options, prop, end, easing, unit ) {
		this.elem = elem;
		this.prop = prop;
		this.easing = easing || jQuery.easing._default;
		this.options = options;
		this.start = this.now = this.cur();
		this.end = end;
		this.unit = unit || ( jQuery.cssNumber[ prop ] ? "" : "px" );
	},
	cur: function() {
		var hooks = Tween.propHooks[ this.prop ];

		return hooks && hooks.get ?
			hooks.get( this ) :
			Tween.propHooks._default.get( this );
	},
	run: function( percent ) {
		var eased,
			hooks = Tween.propHooks[ this.prop ];

		if ( this.options.duration ) {
			this.pos = eased = jQuery.easing[ this.easing ](
				percent, this.options.duration * percent, 0, 1, this.options.duration
			);
		} else {
			this.pos = eased = percent;
		}
		this.now = ( this.end - this.start ) * eased + this.start;

		if ( this.options.step ) {
			this.options.step.call( this.elem, this.now, this );
		}

		if ( hooks && hooks.set ) {
			hooks.set( this );
		} else {
			Tween.propHooks._default.set( this );
		}
		return this;
	}
};

Tween.prototype.init.prototype = Tween.prototype;

Tween.propHooks = {
	_default: {
		get: function( tween ) {
			var result;

			// Use a property on the element directly when it is not a DOM element,
			// or when there is no matching style property that exists.
			if ( tween.elem.nodeType !== 1 ||
				tween.elem[ tween.prop ] != null && tween.elem.style[ tween.prop ] == null ) {
				return tween.elem[ tween.prop ];
			}

			// passing an empty string as a 3rd parameter to .css will automatically
			// attempt a parseFloat and fallback to a string if the parse fails
			// so, simple values such as "10px" are parsed to Float.
			// complex values such as "rotate(1rad)" are returned as is.
			result = jQuery.css( tween.elem, tween.prop, "" );

			// Empty strings, null, undefined and "auto" are converted to 0.
			return !result || result === "auto" ? 0 : result;
		},
		set: function( tween ) {

			// use step hook for back compat - use cssHook if its there - use .style if its
			// available and use plain properties where available
			if ( jQuery.fx.step[ tween.prop ] ) {
				jQuery.fx.step[ tween.prop ]( tween );
			} else if ( tween.elem.nodeType === 1 &&
				( tween.elem.style[ jQuery.cssProps[ tween.prop ] ] != null ||
					jQuery.cssHooks[ tween.prop ] ) ) {
				jQuery.style( tween.elem, tween.prop, tween.now + tween.unit );
			} else {
				tween.elem[ tween.prop ] = tween.now;
			}
		}
	}
};

// Support: IE <=9
// Panic based approach to setting things on disconnected nodes

Tween.propHooks.scrollTop = Tween.propHooks.scrollLeft = {
	set: function( tween ) {
		if ( tween.elem.nodeType && tween.elem.parentNode ) {
			tween.elem[ tween.prop ] = tween.now;
		}
	}
};

jQuery.easing = {
	linear: function( p ) {
		return p;
	},
	swing: function( p ) {
		return 0.5 - Math.cos( p * Math.PI ) / 2;
	},
	_default: "swing"
};

jQuery.fx = Tween.prototype.init;

// Back Compat <1.8 extension point
jQuery.fx.step = {};




var
	fxNow, timerId,
	rfxtypes = /^(?:toggle|show|hide)$/,
	rrun = /queueHooks$/;

// Animations created synchronously will run synchronously
function createFxNow() {
	window.setTimeout( function() {
		fxNow = undefined;
	} );
	return ( fxNow = jQuery.now() );
}

// Generate parameters to create a standard animation
function genFx( type, includeWidth ) {
	var which,
		attrs = { height: type },
		i = 0;

	// if we include width, step value is 1 to do all cssExpand values,
	// if we don't include width, step value is 2 to skip over Left and Right
	includeWidth = includeWidth ? 1 : 0;
	for ( ; i < 4 ; i += 2 - includeWidth ) {
		which = cssExpand[ i ];
		attrs[ "margin" + which ] = attrs[ "padding" + which ] = type;
	}

	if ( includeWidth ) {
		attrs.opacity = attrs.width = type;
	}

	return attrs;
}

function createTween( value, prop, animation ) {
	var tween,
		collection = ( Animation.tweeners[ prop ] || [] ).concat( Animation.tweeners[ "*" ] ),
		index = 0,
		length = collection.length;
	for ( ; index < length; index++ ) {
		if ( ( tween = collection[ index ].call( animation, prop, value ) ) ) {

			// we're done with this property
			return tween;
		}
	}
}

function defaultPrefilter( elem, props, opts ) {
	/* jshint validthis: true */
	var prop, value, toggle, tween, hooks, oldfire, display, checkDisplay,
		anim = this,
		orig = {},
		style = elem.style,
		hidden = elem.nodeType && isHidden( elem ),
		dataShow = jQuery._data( elem, "fxshow" );

	// handle queue: false promises
	if ( !opts.queue ) {
		hooks = jQuery._queueHooks( elem, "fx" );
		if ( hooks.unqueued == null ) {
			hooks.unqueued = 0;
			oldfire = hooks.empty.fire;
			hooks.empty.fire = function() {
				if ( !hooks.unqueued ) {
					oldfire();
				}
			};
		}
		hooks.unqueued++;

		anim.always( function() {

			// doing this makes sure that the complete handler will be called
			// before this completes
			anim.always( function() {
				hooks.unqueued--;
				if ( !jQuery.queue( elem, "fx" ).length ) {
					hooks.empty.fire();
				}
			} );
		} );
	}

	// height/width overflow pass
	if ( elem.nodeType === 1 && ( "height" in props || "width" in props ) ) {

		// Make sure that nothing sneaks out
		// Record all 3 overflow attributes because IE does not
		// change the overflow attribute when overflowX and
		// overflowY are set to the same value
		opts.overflow = [ style.overflow, style.overflowX, style.overflowY ];

		// Set display property to inline-block for height/width
		// animations on inline elements that are having width/height animated
		display = jQuery.css( elem, "display" );

		// Test default display if display is currently "none"
		checkDisplay = display === "none" ?
			jQuery._data( elem, "olddisplay" ) || defaultDisplay( elem.nodeName ) : display;

		if ( checkDisplay === "inline" && jQuery.css( elem, "float" ) === "none" ) {

			// inline-level elements accept inline-block;
			// block-level elements need to be inline with layout
			if ( !support.inlineBlockNeedsLayout || defaultDisplay( elem.nodeName ) === "inline" ) {
				style.display = "inline-block";
			} else {
				style.zoom = 1;
			}
		}
	}

	if ( opts.overflow ) {
		style.overflow = "hidden";
		if ( !support.shrinkWrapBlocks() ) {
			anim.always( function() {
				style.overflow = opts.overflow[ 0 ];
				style.overflowX = opts.overflow[ 1 ];
				style.overflowY = opts.overflow[ 2 ];
			} );
		}
	}

	// show/hide pass
	for ( prop in props ) {
		value = props[ prop ];
		if ( rfxtypes.exec( value ) ) {
			delete props[ prop ];
			toggle = toggle || value === "toggle";
			if ( value === ( hidden ? "hide" : "show" ) ) {

				// If there is dataShow left over from a stopped hide or show
				// and we are going to proceed with show, we should pretend to be hidden
				if ( value === "show" && dataShow && dataShow[ prop ] !== undefined ) {
					hidden = true;
				} else {
					continue;
				}
			}
			orig[ prop ] = dataShow && dataShow[ prop ] || jQuery.style( elem, prop );

		// Any non-fx value stops us from restoring the original display value
		} else {
			display = undefined;
		}
	}

	if ( !jQuery.isEmptyObject( orig ) ) {
		if ( dataShow ) {
			if ( "hidden" in dataShow ) {
				hidden = dataShow.hidden;
			}
		} else {
			dataShow = jQuery._data( elem, "fxshow", {} );
		}

		// store state if its toggle - enables .stop().toggle() to "reverse"
		if ( toggle ) {
			dataShow.hidden = !hidden;
		}
		if ( hidden ) {
			jQuery( elem ).show();
		} else {
			anim.done( function() {
				jQuery( elem ).hide();
			} );
		}
		anim.done( function() {
			var prop;
			jQuery._removeData( elem, "fxshow" );
			for ( prop in orig ) {
				jQuery.style( elem, prop, orig[ prop ] );
			}
		} );
		for ( prop in orig ) {
			tween = createTween( hidden ? dataShow[ prop ] : 0, prop, anim );

			if ( !( prop in dataShow ) ) {
				dataShow[ prop ] = tween.start;
				if ( hidden ) {
					tween.end = tween.start;
					tween.start = prop === "width" || prop === "height" ? 1 : 0;
				}
			}
		}

	// If this is a noop like .hide().hide(), restore an overwritten display value
	} else if ( ( display === "none" ? defaultDisplay( elem.nodeName ) : display ) === "inline" ) {
		style.display = display;
	}
}

function propFilter( props, specialEasing ) {
	var index, name, easing, value, hooks;

	// camelCase, specialEasing and expand cssHook pass
	for ( index in props ) {
		name = jQuery.camelCase( index );
		easing = specialEasing[ name ];
		value = props[ index ];
		if ( jQuery.isArray( value ) ) {
			easing = value[ 1 ];
			value = props[ index ] = value[ 0 ];
		}

		if ( index !== name ) {
			props[ name ] = value;
			delete props[ index ];
		}

		hooks = jQuery.cssHooks[ name ];
		if ( hooks && "expand" in hooks ) {
			value = hooks.expand( value );
			delete props[ name ];

			// not quite $.extend, this wont overwrite keys already present.
			// also - reusing 'index' from above because we have the correct "name"
			for ( index in value ) {
				if ( !( index in props ) ) {
					props[ index ] = value[ index ];
					specialEasing[ index ] = easing;
				}
			}
		} else {
			specialEasing[ name ] = easing;
		}
	}
}

function Animation( elem, properties, options ) {
	var result,
		stopped,
		index = 0,
		length = Animation.prefilters.length,
		deferred = jQuery.Deferred().always( function() {

			// don't match elem in the :animated selector
			delete tick.elem;
		} ),
		tick = function() {
			if ( stopped ) {
				return false;
			}
			var currentTime = fxNow || createFxNow(),
				remaining = Math.max( 0, animation.startTime + animation.duration - currentTime ),

				// Support: Android 2.3
				// Archaic crash bug won't allow us to use `1 - ( 0.5 || 0 )` (#12497)
				temp = remaining / animation.duration || 0,
				percent = 1 - temp,
				index = 0,
				length = animation.tweens.length;

			for ( ; index < length ; index++ ) {
				animation.tweens[ index ].run( percent );
			}

			deferred.notifyWith( elem, [ animation, percent, remaining ] );

			if ( percent < 1 && length ) {
				return remaining;
			} else {
				deferred.resolveWith( elem, [ animation ] );
				return false;
			}
		},
		animation = deferred.promise( {
			elem: elem,
			props: jQuery.extend( {}, properties ),
			opts: jQuery.extend( true, {
				specialEasing: {},
				easing: jQuery.easing._default
			}, options ),
			originalProperties: properties,
			originalOptions: options,
			startTime: fxNow || createFxNow(),
			duration: options.duration,
			tweens: [],
			createTween: function( prop, end ) {
				var tween = jQuery.Tween( elem, animation.opts, prop, end,
						animation.opts.specialEasing[ prop ] || animation.opts.easing );
				animation.tweens.push( tween );
				return tween;
			},
			stop: function( gotoEnd ) {
				var index = 0,

					// if we are going to the end, we want to run all the tweens
					// otherwise we skip this part
					length = gotoEnd ? animation.tweens.length : 0;
				if ( stopped ) {
					return this;
				}
				stopped = true;
				for ( ; index < length ; index++ ) {
					animation.tweens[ index ].run( 1 );
				}

				// resolve when we played the last frame
				// otherwise, reject
				if ( gotoEnd ) {
					deferred.notifyWith( elem, [ animation, 1, 0 ] );
					deferred.resolveWith( elem, [ animation, gotoEnd ] );
				} else {
					deferred.rejectWith( elem, [ animation, gotoEnd ] );
				}
				return this;
			}
		} ),
		props = animation.props;

	propFilter( props, animation.opts.specialEasing );

	for ( ; index < length ; index++ ) {
		result = Animation.prefilters[ index ].call( animation, elem, props, animation.opts );
		if ( result ) {
			if ( jQuery.isFunction( result.stop ) ) {
				jQuery._queueHooks( animation.elem, animation.opts.queue ).stop =
					jQuery.proxy( result.stop, result );
			}
			return result;
		}
	}

	jQuery.map( props, createTween, animation );

	if ( jQuery.isFunction( animation.opts.start ) ) {
		animation.opts.start.call( elem, animation );
	}

	jQuery.fx.timer(
		jQuery.extend( tick, {
			elem: elem,
			anim: animation,
			queue: animation.opts.queue
		} )
	);

	// attach callbacks from options
	return animation.progress( animation.opts.progress )
		.done( animation.opts.done, animation.opts.complete )
		.fail( animation.opts.fail )
		.always( animation.opts.always );
}

jQuery.Animation = jQuery.extend( Animation, {

	tweeners: {
		"*": [ function( prop, value ) {
			var tween = this.createTween( prop, value );
			adjustCSS( tween.elem, prop, rcssNum.exec( value ), tween );
			return tween;
		} ]
	},

	tweener: function( props, callback ) {
		if ( jQuery.isFunction( props ) ) {
			callback = props;
			props = [ "*" ];
		} else {
			props = props.match( rnotwhite );
		}

		var prop,
			index = 0,
			length = props.length;

		for ( ; index < length ; index++ ) {
			prop = props[ index ];
			Animation.tweeners[ prop ] = Animation.tweeners[ prop ] || [];
			Animation.tweeners[ prop ].unshift( callback );
		}
	},

	prefilters: [ defaultPrefilter ],

	prefilter: function( callback, prepend ) {
		if ( prepend ) {
			Animation.prefilters.unshift( callback );
		} else {
			Animation.prefilters.push( callback );
		}
	}
} );

jQuery.speed = function( speed, easing, fn ) {
	var opt = speed && typeof speed === "object" ? jQuery.extend( {}, speed ) : {
		complete: fn || !fn && easing ||
			jQuery.isFunction( speed ) && speed,
		duration: speed,
		easing: fn && easing || easing && !jQuery.isFunction( easing ) && easing
	};

	opt.duration = jQuery.fx.off ? 0 : typeof opt.duration === "number" ? opt.duration :
		opt.duration in jQuery.fx.speeds ?
			jQuery.fx.speeds[ opt.duration ] : jQuery.fx.speeds._default;

	// normalize opt.queue - true/undefined/null -> "fx"
	if ( opt.queue == null || opt.queue === true ) {
		opt.queue = "fx";
	}

	// Queueing
	opt.old = opt.complete;

	opt.complete = function() {
		if ( jQuery.isFunction( opt.old ) ) {
			opt.old.call( this );
		}

		if ( opt.queue ) {
			jQuery.dequeue( this, opt.queue );
		}
	};

	return opt;
};

jQuery.fn.extend( {
	fadeTo: function( speed, to, easing, callback ) {

		// show any hidden elements after setting opacity to 0
		return this.filter( isHidden ).css( "opacity", 0 ).show()

			// animate to the value specified
			.end().animate( { opacity: to }, speed, easing, callback );
	},
	animate: function( prop, speed, easing, callback ) {
		var empty = jQuery.isEmptyObject( prop ),
			optall = jQuery.speed( speed, easing, callback ),
			doAnimation = function() {

				// Operate on a copy of prop so per-property easing won't be lost
				var anim = Animation( this, jQuery.extend( {}, prop ), optall );

				// Empty animations, or finishing resolves immediately
				if ( empty || jQuery._data( this, "finish" ) ) {
					anim.stop( true );
				}
			};
			doAnimation.finish = doAnimation;

		return empty || optall.queue === false ?
			this.each( doAnimation ) :
			this.queue( optall.queue, doAnimation );
	},
	stop: function( type, clearQueue, gotoEnd ) {
		var stopQueue = function( hooks ) {
			var stop = hooks.stop;
			delete hooks.stop;
			stop( gotoEnd );
		};

		if ( typeof type !== "string" ) {
			gotoEnd = clearQueue;
			clearQueue = type;
			type = undefined;
		}
		if ( clearQueue && type !== false ) {
			this.queue( type || "fx", [] );
		}

		return this.each( function() {
			var dequeue = true,
				index = type != null && type + "queueHooks",
				timers = jQuery.timers,
				data = jQuery._data( this );

			if ( index ) {
				if ( data[ index ] && data[ index ].stop ) {
					stopQueue( data[ index ] );
				}
			} else {
				for ( index in data ) {
					if ( data[ index ] && data[ index ].stop && rrun.test( index ) ) {
						stopQueue( data[ index ] );
					}
				}
			}

			for ( index = timers.length; index--; ) {
				if ( timers[ index ].elem === this &&
					( type == null || timers[ index ].queue === type ) ) {

					timers[ index ].anim.stop( gotoEnd );
					dequeue = false;
					timers.splice( index, 1 );
				}
			}

			// start the next in the queue if the last step wasn't forced
			// timers currently will call their complete callbacks, which will dequeue
			// but only if they were gotoEnd
			if ( dequeue || !gotoEnd ) {
				jQuery.dequeue( this, type );
			}
		} );
	},
	finish: function( type ) {
		if ( type !== false ) {
			type = type || "fx";
		}
		return this.each( function() {
			var index,
				data = jQuery._data( this ),
				queue = data[ type + "queue" ],
				hooks = data[ type + "queueHooks" ],
				timers = jQuery.timers,
				length = queue ? queue.length : 0;

			// enable finishing flag on private data
			data.finish = true;

			// empty the queue first
			jQuery.queue( this, type, [] );

			if ( hooks && hooks.stop ) {
				hooks.stop.call( this, true );
			}

			// look for any active animations, and finish them
			for ( index = timers.length; index--; ) {
				if ( timers[ index ].elem === this && timers[ index ].queue === type ) {
					timers[ index ].anim.stop( true );
					timers.splice( index, 1 );
				}
			}

			// look for any animations in the old queue and finish them
			for ( index = 0; index < length; index++ ) {
				if ( queue[ index ] && queue[ index ].finish ) {
					queue[ index ].finish.call( this );
				}
			}

			// turn off finishing flag
			delete data.finish;
		} );
	}
} );

jQuery.each( [ "toggle", "show", "hide" ], function( i, name ) {
	var cssFn = jQuery.fn[ name ];
	jQuery.fn[ name ] = function( speed, easing, callback ) {
		return speed == null || typeof speed === "boolean" ?
			cssFn.apply( this, arguments ) :
			this.animate( genFx( name, true ), speed, easing, callback );
	};
} );

// Generate shortcuts for custom animations
jQuery.each( {
	slideDown: genFx( "show" ),
	slideUp: genFx( "hide" ),
	slideToggle: genFx( "toggle" ),
	fadeIn: { opacity: "show" },
	fadeOut: { opacity: "hide" },
	fadeToggle: { opacity: "toggle" }
}, function( name, props ) {
	jQuery.fn[ name ] = function( speed, easing, callback ) {
		return this.animate( props, speed, easing, callback );
	};
} );

jQuery.timers = [];
jQuery.fx.tick = function() {
	var timer,
		timers = jQuery.timers,
		i = 0;

	fxNow = jQuery.now();

	for ( ; i < timers.length; i++ ) {
		timer = timers[ i ];

		// Checks the timer has not already been removed
		if ( !timer() && timers[ i ] === timer ) {
			timers.splice( i--, 1 );
		}
	}

	if ( !timers.length ) {
		jQuery.fx.stop();
	}
	fxNow = undefined;
};

jQuery.fx.timer = function( timer ) {
	jQuery.timers.push( timer );
	if ( timer() ) {
		jQuery.fx.start();
	} else {
		jQuery.timers.pop();
	}
};

jQuery.fx.interval = 13;

jQuery.fx.start = function() {
	if ( !timerId ) {
		timerId = window.setInterval( jQuery.fx.tick, jQuery.fx.interval );
	}
};

jQuery.fx.stop = function() {
	window.clearInterval( timerId );
	timerId = null;
};

jQuery.fx.speeds = {
	slow: 600,
	fast: 200,

	// Default speed
	_default: 400
};


// Based off of the plugin by Clint Helfers, with permission.
// http://web.archive.org/web/20100324014747/http://blindsignals.com/index.php/2009/07/jquery-delay/
jQuery.fn.delay = function( time, type ) {
	time = jQuery.fx ? jQuery.fx.speeds[ time ] || time : time;
	type = type || "fx";

	return this.queue( type, function( next, hooks ) {
		var timeout = window.setTimeout( next, time );
		hooks.stop = function() {
			window.clearTimeout( timeout );
		};
	} );
};


( function() {
	var a,
		input = document.createElement( "input" ),
		div = document.createElement( "div" ),
		select = document.createElement( "select" ),
		opt = select.appendChild( document.createElement( "option" ) );

	// Setup
	div = document.createElement( "div" );
	div.setAttribute( "className", "t" );
	div.innerHTML = "  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>";
	a = div.getElementsByTagName( "a" )[ 0 ];

	// Support: Windows Web Apps (WWA)
	// `type` must use .setAttribute for WWA (#14901)
	input.setAttribute( "type", "checkbox" );
	div.appendChild( input );

	a = div.getElementsByTagName( "a" )[ 0 ];

	// First batch of tests.
	a.style.cssText = "top:1px";

	// Test setAttribute on camelCase class.
	// If it works, we need attrFixes when doing get/setAttribute (ie6/7)
	support.getSetAttribute = div.className !== "t";

	// Get the style information from getAttribute
	// (IE uses .cssText instead)
	support.style = /top/.test( a.getAttribute( "style" ) );

	// Make sure that URLs aren't manipulated
	// (IE normalizes it by default)
	support.hrefNormalized = a.getAttribute( "href" ) === "/a";

	// Check the default checkbox/radio value ("" on WebKit; "on" elsewhere)
	support.checkOn = !!input.value;

	// Make sure that a selected-by-default option has a working selected property.
	// (WebKit defaults to false instead of true, IE too, if it's in an optgroup)
	support.optSelected = opt.selected;

	// Tests for enctype support on a form (#6743)
	support.enctype = !!document.createElement( "form" ).enctype;

	// Make sure that the options inside disabled selects aren't marked as disabled
	// (WebKit marks them as disabled)
	select.disabled = true;
	support.optDisabled = !opt.disabled;

	// Support: IE8 only
	// Check if we can trust getAttribute("value")
	input = document.createElement( "input" );
	input.setAttribute( "value", "" );
	support.input = input.getAttribute( "value" ) === "";

	// Check if an input maintains its value after becoming a radio
	input.value = "t";
	input.setAttribute( "type", "radio" );
	support.radioValue = input.value === "t";
} )();


var rreturn = /\r/g;

jQuery.fn.extend( {
	val: function( value ) {
		var hooks, ret, isFunction,
			elem = this[ 0 ];

		if ( !arguments.length ) {
			if ( elem ) {
				hooks = jQuery.valHooks[ elem.type ] ||
					jQuery.valHooks[ elem.nodeName.toLowerCase() ];

				if (
					hooks &&
					"get" in hooks &&
					( ret = hooks.get( elem, "value" ) ) !== undefined
				) {
					return ret;
				}

				ret = elem.value;

				return typeof ret === "string" ?

					// handle most common string cases
					ret.replace( rreturn, "" ) :

					// handle cases where value is null/undef or number
					ret == null ? "" : ret;
			}

			return;
		}

		isFunction = jQuery.isFunction( value );

		return this.each( function( i ) {
			var val;

			if ( this.nodeType !== 1 ) {
				return;
			}

			if ( isFunction ) {
				val = value.call( this, i, jQuery( this ).val() );
			} else {
				val = value;
			}

			// Treat null/undefined as ""; convert numbers to string
			if ( val == null ) {
				val = "";
			} else if ( typeof val === "number" ) {
				val += "";
			} else if ( jQuery.isArray( val ) ) {
				val = jQuery.map( val, function( value ) {
					return value == null ? "" : value + "";
				} );
			}

			hooks = jQuery.valHooks[ this.type ] || jQuery.valHooks[ this.nodeName.toLowerCase() ];

			// If set returns undefined, fall back to normal setting
			if ( !hooks || !( "set" in hooks ) || hooks.set( this, val, "value" ) === undefined ) {
				this.value = val;
			}
		} );
	}
} );

jQuery.extend( {
	valHooks: {
		option: {
			get: function( elem ) {
				var val = jQuery.find.attr( elem, "value" );
				return val != null ?
					val :

					// Support: IE10-11+
					// option.text throws exceptions (#14686, #14858)
					jQuery.trim( jQuery.text( elem ) );
			}
		},
		select: {
			get: function( elem ) {
				var value, option,
					options = elem.options,
					index = elem.selectedIndex,
					one = elem.type === "select-one" || index < 0,
					values = one ? null : [],
					max = one ? index + 1 : options.length,
					i = index < 0 ?
						max :
						one ? index : 0;

				// Loop through all the selected options
				for ( ; i < max; i++ ) {
					option = options[ i ];

					// oldIE doesn't update selected after form reset (#2551)
					if ( ( option.selected || i === index ) &&

							// Don't return options that are disabled or in a disabled optgroup
							( support.optDisabled ?
								!option.disabled :
								option.getAttribute( "disabled" ) === null ) &&
							( !option.parentNode.disabled ||
								!jQuery.nodeName( option.parentNode, "optgroup" ) ) ) {

						// Get the specific value for the option
						value = jQuery( option ).val();

						// We don't need an array for one selects
						if ( one ) {
							return value;
						}

						// Multi-Selects return an array
						values.push( value );
					}
				}

				return values;
			},

			set: function( elem, value ) {
				var optionSet, option,
					options = elem.options,
					values = jQuery.makeArray( value ),
					i = options.length;

				while ( i-- ) {
					option = options[ i ];

					if ( jQuery.inArray( jQuery.valHooks.option.get( option ), values ) >= 0 ) {

						// Support: IE6
						// When new option element is added to select box we need to
						// force reflow of newly added node in order to workaround delay
						// of initialization properties
						try {
							option.selected = optionSet = true;

						} catch ( _ ) {

							// Will be executed only in IE6
							option.scrollHeight;
						}

					} else {
						option.selected = false;
					}
				}

				// Force browsers to behave consistently when non-matching value is set
				if ( !optionSet ) {
					elem.selectedIndex = -1;
				}

				return options;
			}
		}
	}
} );

// Radios and checkboxes getter/setter
jQuery.each( [ "radio", "checkbox" ], function() {
	jQuery.valHooks[ this ] = {
		set: function( elem, value ) {
			if ( jQuery.isArray( value ) ) {
				return ( elem.checked = jQuery.inArray( jQuery( elem ).val(), value ) > -1 );
			}
		}
	};
	if ( !support.checkOn ) {
		jQuery.valHooks[ this ].get = function( elem ) {
			return elem.getAttribute( "value" ) === null ? "on" : elem.value;
		};
	}
} );




var nodeHook, boolHook,
	attrHandle = jQuery.expr.attrHandle,
	ruseDefault = /^(?:checked|selected)$/i,
	getSetAttribute = support.getSetAttribute,
	getSetInput = support.input;

jQuery.fn.extend( {
	attr: function( name, value ) {
		return access( this, jQuery.attr, name, value, arguments.length > 1 );
	},

	removeAttr: function( name ) {
		return this.each( function() {
			jQuery.removeAttr( this, name );
		} );
	}
} );

jQuery.extend( {
	attr: function( elem, name, value ) {
		var ret, hooks,
			nType = elem.nodeType;

		// Don't get/set attributes on text, comment and attribute nodes
		if ( nType === 3 || nType === 8 || nType === 2 ) {
			return;
		}

		// Fallback to prop when attributes are not supported
		if ( typeof elem.getAttribute === "undefined" ) {
			return jQuery.prop( elem, name, value );
		}

		// All attributes are lowercase
		// Grab necessary hook if one is defined
		if ( nType !== 1 || !jQuery.isXMLDoc( elem ) ) {
			name = name.toLowerCase();
			hooks = jQuery.attrHooks[ name ] ||
				( jQuery.expr.match.bool.test( name ) ? boolHook : nodeHook );
		}

		if ( value !== undefined ) {
			if ( value === null ) {
				jQuery.removeAttr( elem, name );
				return;
			}

			if ( hooks && "set" in hooks &&
				( ret = hooks.set( elem, value, name ) ) !== undefined ) {
				return ret;
			}

			elem.setAttribute( name, value + "" );
			return value;
		}

		if ( hooks && "get" in hooks && ( ret = hooks.get( elem, name ) ) !== null ) {
			return ret;
		}

		ret = jQuery.find.attr( elem, name );

		// Non-existent attributes return null, we normalize to undefined
		return ret == null ? undefined : ret;
	},

	attrHooks: {
		type: {
			set: function( elem, value ) {
				if ( !support.radioValue && value === "radio" &&
					jQuery.nodeName( elem, "input" ) ) {

					// Setting the type on a radio button after the value resets the value in IE8-9
					// Reset value to default in case type is set after value during creation
					var val = elem.value;
					elem.setAttribute( "type", value );
					if ( val ) {
						elem.value = val;
					}
					return value;
				}
			}
		}
	},

	removeAttr: function( elem, value ) {
		var name, propName,
			i = 0,
			attrNames = value && value.match( rnotwhite );

		if ( attrNames && elem.nodeType === 1 ) {
			while ( ( name = attrNames[ i++ ] ) ) {
				propName = jQuery.propFix[ name ] || name;

				// Boolean attributes get special treatment (#10870)
				if ( jQuery.expr.match.bool.test( name ) ) {

					// Set corresponding property to false
					if ( getSetInput && getSetAttribute || !ruseDefault.test( name ) ) {
						elem[ propName ] = false;

					// Support: IE<9
					// Also clear defaultChecked/defaultSelected (if appropriate)
					} else {
						elem[ jQuery.camelCase( "default-" + name ) ] =
							elem[ propName ] = false;
					}

				// See #9699 for explanation of this approach (setting first, then removal)
				} else {
					jQuery.attr( elem, name, "" );
				}

				elem.removeAttribute( getSetAttribute ? name : propName );
			}
		}
	}
} );

// Hooks for boolean attributes
boolHook = {
	set: function( elem, value, name ) {
		if ( value === false ) {

			// Remove boolean attributes when set to false
			jQuery.removeAttr( elem, name );
		} else if ( getSetInput && getSetAttribute || !ruseDefault.test( name ) ) {

			// IE<8 needs the *property* name
			elem.setAttribute( !getSetAttribute && jQuery.propFix[ name ] || name, name );

		} else {

			// Support: IE<9
			// Use defaultChecked and defaultSelected for oldIE
			elem[ jQuery.camelCase( "default-" + name ) ] = elem[ name ] = true;
		}
		return name;
	}
};

jQuery.each( jQuery.expr.match.bool.source.match( /\w+/g ), function( i, name ) {
	var getter = attrHandle[ name ] || jQuery.find.attr;

	if ( getSetInput && getSetAttribute || !ruseDefault.test( name ) ) {
		attrHandle[ name ] = function( elem, name, isXML ) {
			var ret, handle;
			if ( !isXML ) {

				// Avoid an infinite loop by temporarily removing this function from the getter
				handle = attrHandle[ name ];
				attrHandle[ name ] = ret;
				ret = getter( elem, name, isXML ) != null ?
					name.toLowerCase() :
					null;
				attrHandle[ name ] = handle;
			}
			return ret;
		};
	} else {
		attrHandle[ name ] = function( elem, name, isXML ) {
			if ( !isXML ) {
				return elem[ jQuery.camelCase( "default-" + name ) ] ?
					name.toLowerCase() :
					null;
			}
		};
	}
} );

// fix oldIE attroperties
if ( !getSetInput || !getSetAttribute ) {
	jQuery.attrHooks.value = {
		set: function( elem, value, name ) {
			if ( jQuery.nodeName( elem, "input" ) ) {

				// Does not return so that setAttribute is also used
				elem.defaultValue = value;
			} else {

				// Use nodeHook if defined (#1954); otherwise setAttribute is fine
				return nodeHook && nodeHook.set( elem, value, name );
			}
		}
	};
}

// IE6/7 do not support getting/setting some attributes with get/setAttribute
if ( !getSetAttribute ) {

	// Use this for any attribute in IE6/7
	// This fixes almost every IE6/7 issue
	nodeHook = {
		set: function( elem, value, name ) {

			// Set the existing or create a new attribute node
			var ret = elem.getAttributeNode( name );
			if ( !ret ) {
				elem.setAttributeNode(
					( ret = elem.ownerDocument.createAttribute( name ) )
				);
			}

			ret.value = value += "";

			// Break association with cloned elements by also using setAttribute (#9646)
			if ( name === "value" || value === elem.getAttribute( name ) ) {
				return value;
			}
		}
	};

	// Some attributes are constructed with empty-string values when not defined
	attrHandle.id = attrHandle.name = attrHandle.coords =
		function( elem, name, isXML ) {
			var ret;
			if ( !isXML ) {
				return ( ret = elem.getAttributeNode( name ) ) && ret.value !== "" ?
					ret.value :
					null;
			}
		};

	// Fixing value retrieval on a button requires this module
	jQuery.valHooks.button = {
		get: function( elem, name ) {
			var ret = elem.getAttributeNode( name );
			if ( ret && ret.specified ) {
				return ret.value;
			}
		},
		set: nodeHook.set
	};

	// Set contenteditable to false on removals(#10429)
	// Setting to empty string throws an error as an invalid value
	jQuery.attrHooks.contenteditable = {
		set: function( elem, value, name ) {
			nodeHook.set( elem, value === "" ? false : value, name );
		}
	};

	// Set width and height to auto instead of 0 on empty string( Bug #8150 )
	// This is for removals
	jQuery.each( [ "width", "height" ], function( i, name ) {
		jQuery.attrHooks[ name ] = {
			set: function( elem, value ) {
				if ( value === "" ) {
					elem.setAttribute( name, "auto" );
					return value;
				}
			}
		};
	} );
}

if ( !support.style ) {
	jQuery.attrHooks.style = {
		get: function( elem ) {

			// Return undefined in the case of empty string
			// Note: IE uppercases css property names, but if we were to .toLowerCase()
			// .cssText, that would destroy case sensitivity in URL's, like in "background"
			return elem.style.cssText || undefined;
		},
		set: function( elem, value ) {
			return ( elem.style.cssText = value + "" );
		}
	};
}




var rfocusable = /^(?:input|select|textarea|button|object)$/i,
	rclickable = /^(?:a|area)$/i;

jQuery.fn.extend( {
	prop: function( name, value ) {
		return access( this, jQuery.prop, name, value, arguments.length > 1 );
	},

	removeProp: function( name ) {
		name = jQuery.propFix[ name ] || name;
		return this.each( function() {

			// try/catch handles cases where IE balks (such as removing a property on window)
			try {
				this[ name ] = undefined;
				delete this[ name ];
			} catch ( e ) {}
		} );
	}
} );

jQuery.extend( {
	prop: function( elem, name, value ) {
		var ret, hooks,
			nType = elem.nodeType;

		// Don't get/set properties on text, comment and attribute nodes
		if ( nType === 3 || nType === 8 || nType === 2 ) {
			return;
		}

		if ( nType !== 1 || !jQuery.isXMLDoc( elem ) ) {

			// Fix name and attach hooks
			name = jQuery.propFix[ name ] || name;
			hooks = jQuery.propHooks[ name ];
		}

		if ( value !== undefined ) {
			if ( hooks && "set" in hooks &&
				( ret = hooks.set( elem, value, name ) ) !== undefined ) {
				return ret;
			}

			return ( elem[ name ] = value );
		}

		if ( hooks && "get" in hooks && ( ret = hooks.get( elem, name ) ) !== null ) {
			return ret;
		}

		return elem[ name ];
	},

	propHooks: {
		tabIndex: {
			get: function( elem ) {

				// elem.tabIndex doesn't always return the
				// correct value when it hasn't been explicitly set
				// http://fluidproject.org/blog/2008/01/09/getting-setting-and-removing-tabindex-values-with-javascript/
				// Use proper attribute retrieval(#12072)
				var tabindex = jQuery.find.attr( elem, "tabindex" );

				return tabindex ?
					parseInt( tabindex, 10 ) :
					rfocusable.test( elem.nodeName ) ||
						rclickable.test( elem.nodeName ) && elem.href ?
							0 :
							-1;
			}
		}
	},

	propFix: {
		"for": "htmlFor",
		"class": "className"
	}
} );

// Some attributes require a special call on IE
// http://msdn.microsoft.com/en-us/library/ms536429%28VS.85%29.aspx
if ( !support.hrefNormalized ) {

	// href/src property should get the full normalized URL (#10299/#12915)
	jQuery.each( [ "href", "src" ], function( i, name ) {
		jQuery.propHooks[ name ] = {
			get: function( elem ) {
				return elem.getAttribute( name, 4 );
			}
		};
	} );
}

// Support: Safari, IE9+
// mis-reports the default selected property of an option
// Accessing the parent's selectedIndex property fixes it
if ( !support.optSelected ) {
	jQuery.propHooks.selected = {
		get: function( elem ) {
			var parent = elem.parentNode;

			if ( parent ) {
				parent.selectedIndex;

				// Make sure that it also works with optgroups, see #5701
				if ( parent.parentNode ) {
					parent.parentNode.selectedIndex;
				}
			}
			return null;
		}
	};
}

jQuery.each( [
	"tabIndex",
	"readOnly",
	"maxLength",
	"cellSpacing",
	"cellPadding",
	"rowSpan",
	"colSpan",
	"useMap",
	"frameBorder",
	"contentEditable"
], function() {
	jQuery.propFix[ this.toLowerCase() ] = this;
} );

// IE6/7 call enctype encoding
if ( !support.enctype ) {
	jQuery.propFix.enctype = "encoding";
}




var rclass = /[\t\r\n\f]/g;

function getClass( elem ) {
	return jQuery.attr( elem, "class" ) || "";
}

jQuery.fn.extend( {
	addClass: function( value ) {
		var classes, elem, cur, curValue, clazz, j, finalValue,
			i = 0;

		if ( jQuery.isFunction( value ) ) {
			return this.each( function( j ) {
				jQuery( this ).addClass( value.call( this, j, getClass( this ) ) );
			} );
		}

		if ( typeof value === "string" && value ) {
			classes = value.match( rnotwhite ) || [];

			while ( ( elem = this[ i++ ] ) ) {
				curValue = getClass( elem );
				cur = elem.nodeType === 1 &&
					( " " + curValue + " " ).replace( rclass, " " );

				if ( cur ) {
					j = 0;
					while ( ( clazz = classes[ j++ ] ) ) {
						if ( cur.indexOf( " " + clazz + " " ) < 0 ) {
							cur += clazz + " ";
						}
					}

					// only assign if different to avoid unneeded rendering.
					finalValue = jQuery.trim( cur );
					if ( curValue !== finalValue ) {
						jQuery.attr( elem, "class", finalValue );
					}
				}
			}
		}

		return this;
	},

	removeClass: function( value ) {
		var classes, elem, cur, curValue, clazz, j, finalValue,
			i = 0;

		if ( jQuery.isFunction( value ) ) {
			return this.each( function( j ) {
				jQuery( this ).removeClass( value.call( this, j, getClass( this ) ) );
			} );
		}

		if ( !arguments.length ) {
			return this.attr( "class", "" );
		}

		if ( typeof value === "string" && value ) {
			classes = value.match( rnotwhite ) || [];

			while ( ( elem = this[ i++ ] ) ) {
				curValue = getClass( elem );

				// This expression is here for better compressibility (see addClass)
				cur = elem.nodeType === 1 &&
					( " " + curValue + " " ).replace( rclass, " " );

				if ( cur ) {
					j = 0;
					while ( ( clazz = classes[ j++ ] ) ) {

						// Remove *all* instances
						while ( cur.indexOf( " " + clazz + " " ) > -1 ) {
							cur = cur.replace( " " + clazz + " ", " " );
						}
					}

					// Only assign if different to avoid unneeded rendering.
					finalValue = jQuery.trim( cur );
					if ( curValue !== finalValue ) {
						jQuery.attr( elem, "class", finalValue );
					}
				}
			}
		}

		return this;
	},

	toggleClass: function( value, stateVal ) {
		var type = typeof value;

		if ( typeof stateVal === "boolean" && type === "string" ) {
			return stateVal ? this.addClass( value ) : this.removeClass( value );
		}

		if ( jQuery.isFunction( value ) ) {
			return this.each( function( i ) {
				jQuery( this ).toggleClass(
					value.call( this, i, getClass( this ), stateVal ),
					stateVal
				);
			} );
		}

		return this.each( function() {
			var className, i, self, classNames;

			if ( type === "string" ) {

				// Toggle individual class names
				i = 0;
				self = jQuery( this );
				classNames = value.match( rnotwhite ) || [];

				while ( ( className = classNames[ i++ ] ) ) {

					// Check each className given, space separated list
					if ( self.hasClass( className ) ) {
						self.removeClass( className );
					} else {
						self.addClass( className );
					}
				}

			// Toggle whole class name
			} else if ( value === undefined || type === "boolean" ) {
				className = getClass( this );
				if ( className ) {

					// store className if set
					jQuery._data( this, "__className__", className );
				}

				// If the element has a class name or if we're passed "false",
				// then remove the whole classname (if there was one, the above saved it).
				// Otherwise bring back whatever was previously saved (if anything),
				// falling back to the empty string if nothing was stored.
				jQuery.attr( this, "class",
					className || value === false ?
					"" :
					jQuery._data( this, "__className__" ) || ""
				);
			}
		} );
	},

	hasClass: function( selector ) {
		var className, elem,
			i = 0;

		className = " " + selector + " ";
		while ( ( elem = this[ i++ ] ) ) {
			if ( elem.nodeType === 1 &&
				( " " + getClass( elem ) + " " ).replace( rclass, " " )
					.indexOf( className ) > -1
			) {
				return true;
			}
		}

		return false;
	}
} );




// Return jQuery for attributes-only inclusion


jQuery.each( ( "blur focus focusin focusout load resize scroll unload click dblclick " +
	"mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave " +
	"change select submit keydown keypress keyup error contextmenu" ).split( " " ),
	function( i, name ) {

	// Handle event binding
	jQuery.fn[ name ] = function( data, fn ) {
		return arguments.length > 0 ?
			this.on( name, null, data, fn ) :
			this.trigger( name );
	};
} );

jQuery.fn.extend( {
	hover: function( fnOver, fnOut ) {
		return this.mouseenter( fnOver ).mouseleave( fnOut || fnOver );
	}
} );


var location = window.location;

var nonce = jQuery.now();

var rquery = ( /\?/ );



var rvalidtokens = /(,)|(\[|{)|(}|])|"(?:[^"\\\r\n]|\\["\\\/bfnrt]|\\u[\da-fA-F]{4})*"\s*:?|true|false|null|-?(?!0\d)\d+(?:\.\d+|)(?:[eE][+-]?\d+|)/g;

jQuery.parseJSON = function( data ) {

	// Attempt to parse using the native JSON parser first
	if ( window.JSON && window.JSON.parse ) {

		// Support: Android 2.3
		// Workaround failure to string-cast null input
		return window.JSON.parse( data + "" );
	}

	var requireNonComma,
		depth = null,
		str = jQuery.trim( data + "" );

	// Guard against invalid (and possibly dangerous) input by ensuring that nothing remains
	// after removing valid tokens
	return str && !jQuery.trim( str.replace( rvalidtokens, function( token, comma, open, close ) {

		// Force termination if we see a misplaced comma
		if ( requireNonComma && comma ) {
			depth = 0;
		}

		// Perform no more replacements after returning to outermost depth
		if ( depth === 0 ) {
			return token;
		}

		// Commas must not follow "[", "{", or ","
		requireNonComma = open || comma;

		// Determine new depth
		// array/object open ("[" or "{"): depth += true - false (increment)
		// array/object close ("]" or "}"): depth += false - true (decrement)
		// other cases ("," or primitive): depth += true - true (numeric cast)
		depth += !close - !open;

		// Remove this token
		return "";
	} ) ) ?
		( Function( "return " + str ) )() :
		jQuery.error( "Invalid JSON: " + data );
};


// Cross-browser xml parsing
jQuery.parseXML = function( data ) {
	var xml, tmp;
	if ( !data || typeof data !== "string" ) {
		return null;
	}
	try {
		if ( window.DOMParser ) { // Standard
			tmp = new window.DOMParser();
			xml = tmp.parseFromString( data, "text/xml" );
		} else { // IE
			xml = new window.ActiveXObject( "Microsoft.XMLDOM" );
			xml.async = "false";
			xml.loadXML( data );
		}
	} catch ( e ) {
		xml = undefined;
	}
	if ( !xml || !xml.documentElement || xml.getElementsByTagName( "parsererror" ).length ) {
		jQuery.error( "Invalid XML: " + data );
	}
	return xml;
};


var
	rhash = /#.*$/,
	rts = /([?&])_=[^&]*/,

	// IE leaves an \r character at EOL
	rheaders = /^(.*?):[ \t]*([^\r\n]*)\r?$/mg,

	// #7653, #8125, #8152: local protocol detection
	rlocalProtocol = /^(?:about|app|app-storage|.+-extension|file|res|widget):$/,
	rnoContent = /^(?:GET|HEAD)$/,
	rprotocol = /^\/\//,
	rurl = /^([\w.+-]+:)(?:\/\/(?:[^\/?#]*@|)([^\/?#:]*)(?::(\d+)|)|)/,

	/* Prefilters
	 * 1) They are useful to introduce custom dataTypes (see ajax/jsonp.js for an example)
	 * 2) These are called:
	 *    - BEFORE asking for a transport
	 *    - AFTER param serialization (s.data is a string if s.processData is true)
	 * 3) key is the dataType
	 * 4) the catchall symbol "*" can be used
	 * 5) execution will start with transport dataType and THEN continue down to "*" if needed
	 */
	prefilters = {},

	/* Transports bindings
	 * 1) key is the dataType
	 * 2) the catchall symbol "*" can be used
	 * 3) selection will start with transport dataType and THEN go to "*" if needed
	 */
	transports = {},

	// Avoid comment-prolog char sequence (#10098); must appease lint and evade compression
	allTypes = "*/".concat( "*" ),

	// Document location
	ajaxLocation = location.href,

	// Segment location into parts
	ajaxLocParts = rurl.exec( ajaxLocation.toLowerCase() ) || [];

// Base "constructor" for jQuery.ajaxPrefilter and jQuery.ajaxTransport
function addToPrefiltersOrTransports( structure ) {

	// dataTypeExpression is optional and defaults to "*"
	return function( dataTypeExpression, func ) {

		if ( typeof dataTypeExpression !== "string" ) {
			func = dataTypeExpression;
			dataTypeExpression = "*";
		}

		var dataType,
			i = 0,
			dataTypes = dataTypeExpression.toLowerCase().match( rnotwhite ) || [];

		if ( jQuery.isFunction( func ) ) {

			// For each dataType in the dataTypeExpression
			while ( ( dataType = dataTypes[ i++ ] ) ) {

				// Prepend if requested
				if ( dataType.charAt( 0 ) === "+" ) {
					dataType = dataType.slice( 1 ) || "*";
					( structure[ dataType ] = structure[ dataType ] || [] ).unshift( func );

				// Otherwise append
				} else {
					( structure[ dataType ] = structure[ dataType ] || [] ).push( func );
				}
			}
		}
	};
}

// Base inspection function for prefilters and transports
function inspectPrefiltersOrTransports( structure, options, originalOptions, jqXHR ) {

	var inspected = {},
		seekingTransport = ( structure === transports );

	function inspect( dataType ) {
		var selected;
		inspected[ dataType ] = true;
		jQuery.each( structure[ dataType ] || [], function( _, prefilterOrFactory ) {
			var dataTypeOrTransport = prefilterOrFactory( options, originalOptions, jqXHR );
			if ( typeof dataTypeOrTransport === "string" &&
				!seekingTransport && !inspected[ dataTypeOrTransport ] ) {

				options.dataTypes.unshift( dataTypeOrTransport );
				inspect( dataTypeOrTransport );
				return false;
			} else if ( seekingTransport ) {
				return !( selected = dataTypeOrTransport );
			}
		} );
		return selected;
	}

	return inspect( options.dataTypes[ 0 ] ) || !inspected[ "*" ] && inspect( "*" );
}

// A special extend for ajax options
// that takes "flat" options (not to be deep extended)
// Fixes #9887
function ajaxExtend( target, src ) {
	var deep, key,
		flatOptions = jQuery.ajaxSettings.flatOptions || {};

	for ( key in src ) {
		if ( src[ key ] !== undefined ) {
			( flatOptions[ key ] ? target : ( deep || ( deep = {} ) ) )[ key ] = src[ key ];
		}
	}
	if ( deep ) {
		jQuery.extend( true, target, deep );
	}

	return target;
}

/* Handles responses to an ajax request:
 * - finds the right dataType (mediates between content-type and expected dataType)
 * - returns the corresponding response
 */
function ajaxHandleResponses( s, jqXHR, responses ) {
	var firstDataType, ct, finalDataType, type,
		contents = s.contents,
		dataTypes = s.dataTypes;

	// Remove auto dataType and get content-type in the process
	while ( dataTypes[ 0 ] === "*" ) {
		dataTypes.shift();
		if ( ct === undefined ) {
			ct = s.mimeType || jqXHR.getResponseHeader( "Content-Type" );
		}
	}

	// Check if we're dealing with a known content-type
	if ( ct ) {
		for ( type in contents ) {
			if ( contents[ type ] && contents[ type ].test( ct ) ) {
				dataTypes.unshift( type );
				break;
			}
		}
	}

	// Check to see if we have a response for the expected dataType
	if ( dataTypes[ 0 ] in responses ) {
		finalDataType = dataTypes[ 0 ];
	} else {

		// Try convertible dataTypes
		for ( type in responses ) {
			if ( !dataTypes[ 0 ] || s.converters[ type + " " + dataTypes[ 0 ] ] ) {
				finalDataType = type;
				break;
			}
			if ( !firstDataType ) {
				firstDataType = type;
			}
		}

		// Or just use first one
		finalDataType = finalDataType || firstDataType;
	}

	// If we found a dataType
	// We add the dataType to the list if needed
	// and return the corresponding response
	if ( finalDataType ) {
		if ( finalDataType !== dataTypes[ 0 ] ) {
			dataTypes.unshift( finalDataType );
		}
		return responses[ finalDataType ];
	}
}

/* Chain conversions given the request and the original response
 * Also sets the responseXXX fields on the jqXHR instance
 */
function ajaxConvert( s, response, jqXHR, isSuccess ) {
	var conv2, current, conv, tmp, prev,
		converters = {},

		// Work with a copy of dataTypes in case we need to modify it for conversion
		dataTypes = s.dataTypes.slice();

	// Create converters map with lowercased keys
	if ( dataTypes[ 1 ] ) {
		for ( conv in s.converters ) {
			converters[ conv.toLowerCase() ] = s.converters[ conv ];
		}
	}

	current = dataTypes.shift();

	// Convert to each sequential dataType
	while ( current ) {

		if ( s.responseFields[ current ] ) {
			jqXHR[ s.responseFields[ current ] ] = response;
		}

		// Apply the dataFilter if provided
		if ( !prev && isSuccess && s.dataFilter ) {
			response = s.dataFilter( response, s.dataType );
		}

		prev = current;
		current = dataTypes.shift();

		if ( current ) {

			// There's only work to do if current dataType is non-auto
			if ( current === "*" ) {

				current = prev;

			// Convert response if prev dataType is non-auto and differs from current
			} else if ( prev !== "*" && prev !== current ) {

				// Seek a direct converter
				conv = converters[ prev + " " + current ] || converters[ "* " + current ];

				// If none found, seek a pair
				if ( !conv ) {
					for ( conv2 in converters ) {

						// If conv2 outputs current
						tmp = conv2.split( " " );
						if ( tmp[ 1 ] === current ) {

							// If prev can be converted to accepted input
							conv = converters[ prev + " " + tmp[ 0 ] ] ||
								converters[ "* " + tmp[ 0 ] ];
							if ( conv ) {

								// Condense equivalence converters
								if ( conv === true ) {
									conv = converters[ conv2 ];

								// Otherwise, insert the intermediate dataType
								} else if ( converters[ conv2 ] !== true ) {
									current = tmp[ 0 ];
									dataTypes.unshift( tmp[ 1 ] );
								}
								break;
							}
						}
					}
				}

				// Apply converter (if not an equivalence)
				if ( conv !== true ) {

					// Unless errors are allowed to bubble, catch and return them
					if ( conv && s[ "throws" ] ) { // jscs:ignore requireDotNotation
						response = conv( response );
					} else {
						try {
							response = conv( response );
						} catch ( e ) {
							return {
								state: "parsererror",
								error: conv ? e : "No conversion from " + prev + " to " + current
							};
						}
					}
				}
			}
		}
	}

	return { state: "success", data: response };
}

jQuery.extend( {

	// Counter for holding the number of active queries
	active: 0,

	// Last-Modified header cache for next request
	lastModified: {},
	etag: {},

	ajaxSettings: {
		url: ajaxLocation,
		type: "GET",
		isLocal: rlocalProtocol.test( ajaxLocParts[ 1 ] ),
		global: true,
		processData: true,
		async: true,
		contentType: "application/x-www-form-urlencoded; charset=UTF-8",
		/*
		timeout: 0,
		data: null,
		dataType: null,
		username: null,
		password: null,
		cache: null,
		throws: false,
		traditional: false,
		headers: {},
		*/

		accepts: {
			"*": allTypes,
			text: "text/plain",
			html: "text/html",
			xml: "application/xml, text/xml",
			json: "application/json, text/javascript"
		},

		contents: {
			xml: /\bxml\b/,
			html: /\bhtml/,
			json: /\bjson\b/
		},

		responseFields: {
			xml: "responseXML",
			text: "responseText",
			json: "responseJSON"
		},

		// Data converters
		// Keys separate source (or catchall "*") and destination types with a single space
		converters: {

			// Convert anything to text
			"* text": String,

			// Text to html (true = no transformation)
			"text html": true,

			// Evaluate text as a json expression
			"text json": jQuery.parseJSON,

			// Parse text as xml
			"text xml": jQuery.parseXML
		},

		// For options that shouldn't be deep extended:
		// you can add your own custom options here if
		// and when you create one that shouldn't be
		// deep extended (see ajaxExtend)
		flatOptions: {
			url: true,
			context: true
		}
	},

	// Creates a full fledged settings object into target
	// with both ajaxSettings and settings fields.
	// If target is omitted, writes into ajaxSettings.
	ajaxSetup: function( target, settings ) {
		return settings ?

			// Building a settings object
			ajaxExtend( ajaxExtend( target, jQuery.ajaxSettings ), settings ) :

			// Extending ajaxSettings
			ajaxExtend( jQuery.ajaxSettings, target );
	},

	ajaxPrefilter: addToPrefiltersOrTransports( prefilters ),
	ajaxTransport: addToPrefiltersOrTransports( transports ),

	// Main method
	ajax: function( url, options ) {

		// If url is an object, simulate pre-1.5 signature
		if ( typeof url === "object" ) {
			options = url;
			url = undefined;
		}

		// Force options to be an object
		options = options || {};

		var

			// Cross-domain detection vars
			parts,

			// Loop variable
			i,

			// URL without anti-cache param
			cacheURL,

			// Response headers as string
			responseHeadersString,

			// timeout handle
			timeoutTimer,

			// To know if global events are to be dispatched
			fireGlobals,

			transport,

			// Response headers
			responseHeaders,

			// Create the final options object
			s = jQuery.ajaxSetup( {}, options ),

			// Callbacks context
			callbackContext = s.context || s,

			// Context for global events is callbackContext if it is a DOM node or jQuery collection
			globalEventContext = s.context &&
				( callbackContext.nodeType || callbackContext.jquery ) ?
					jQuery( callbackContext ) :
					jQuery.event,

			// Deferreds
			deferred = jQuery.Deferred(),
			completeDeferred = jQuery.Callbacks( "once memory" ),

			// Status-dependent callbacks
			statusCode = s.statusCode || {},

			// Headers (they are sent all at once)
			requestHeaders = {},
			requestHeadersNames = {},

			// The jqXHR state
			state = 0,

			// Default abort message
			strAbort = "canceled",

			// Fake xhr
			jqXHR = {
				readyState: 0,

				// Builds headers hashtable if needed
				getResponseHeader: function( key ) {
					var match;
					if ( state === 2 ) {
						if ( !responseHeaders ) {
							responseHeaders = {};
							while ( ( match = rheaders.exec( responseHeadersString ) ) ) {
								responseHeaders[ match[ 1 ].toLowerCase() ] = match[ 2 ];
							}
						}
						match = responseHeaders[ key.toLowerCase() ];
					}
					return match == null ? null : match;
				},

				// Raw string
				getAllResponseHeaders: function() {
					return state === 2 ? responseHeadersString : null;
				},

				// Caches the header
				setRequestHeader: function( name, value ) {
					var lname = name.toLowerCase();
					if ( !state ) {
						name = requestHeadersNames[ lname ] = requestHeadersNames[ lname ] || name;
						requestHeaders[ name ] = value;
					}
					return this;
				},

				// Overrides response content-type header
				overrideMimeType: function( type ) {
					if ( !state ) {
						s.mimeType = type;
					}
					return this;
				},

				// Status-dependent callbacks
				statusCode: function( map ) {
					var code;
					if ( map ) {
						if ( state < 2 ) {
							for ( code in map ) {

								// Lazy-add the new callback in a way that preserves old ones
								statusCode[ code ] = [ statusCode[ code ], map[ code ] ];
							}
						} else {

							// Execute the appropriate callbacks
							jqXHR.always( map[ jqXHR.status ] );
						}
					}
					return this;
				},

				// Cancel the request
				abort: function( statusText ) {
					var finalText = statusText || strAbort;
					if ( transport ) {
						transport.abort( finalText );
					}
					done( 0, finalText );
					return this;
				}
			};

		// Attach deferreds
		deferred.promise( jqXHR ).complete = completeDeferred.add;
		jqXHR.success = jqXHR.done;
		jqXHR.error = jqXHR.fail;

		// Remove hash character (#7531: and string promotion)
		// Add protocol if not provided (#5866: IE7 issue with protocol-less urls)
		// Handle falsy url in the settings object (#10093: consistency with old signature)
		// We also use the url parameter if available
		s.url = ( ( url || s.url || ajaxLocation ) + "" )
			.replace( rhash, "" )
			.replace( rprotocol, ajaxLocParts[ 1 ] + "//" );

		// Alias method option to type as per ticket #12004
		s.type = options.method || options.type || s.method || s.type;

		// Extract dataTypes list
		s.dataTypes = jQuery.trim( s.dataType || "*" ).toLowerCase().match( rnotwhite ) || [ "" ];

		// A cross-domain request is in order when we have a protocol:host:port mismatch
		if ( s.crossDomain == null ) {
			parts = rurl.exec( s.url.toLowerCase() );
			s.crossDomain = !!( parts &&
				( parts[ 1 ] !== ajaxLocParts[ 1 ] || parts[ 2 ] !== ajaxLocParts[ 2 ] ||
					( parts[ 3 ] || ( parts[ 1 ] === "http:" ? "80" : "443" ) ) !==
						( ajaxLocParts[ 3 ] || ( ajaxLocParts[ 1 ] === "http:" ? "80" : "443" ) ) )
			);
		}

		// Convert data if not already a string
		if ( s.data && s.processData && typeof s.data !== "string" ) {
			s.data = jQuery.param( s.data, s.traditional );
		}

		// Apply prefilters
		inspectPrefiltersOrTransports( prefilters, s, options, jqXHR );

		// If request was aborted inside a prefilter, stop there
		if ( state === 2 ) {
			return jqXHR;
		}

		// We can fire global events as of now if asked to
		// Don't fire events if jQuery.event is undefined in an AMD-usage scenario (#15118)
		fireGlobals = jQuery.event && s.global;

		// Watch for a new set of requests
		if ( fireGlobals && jQuery.active++ === 0 ) {
			jQuery.event.trigger( "ajaxStart" );
		}

		// Uppercase the type
		s.type = s.type.toUpperCase();

		// Determine if request has content
		s.hasContent = !rnoContent.test( s.type );

		// Save the URL in case we're toying with the If-Modified-Since
		// and/or If-None-Match header later on
		cacheURL = s.url;

		// More options handling for requests with no content
		if ( !s.hasContent ) {

			// If data is available, append data to url
			if ( s.data ) {
				cacheURL = ( s.url += ( rquery.test( cacheURL ) ? "&" : "?" ) + s.data );

				// #9682: remove data so that it's not used in an eventual retry
				delete s.data;
			}

			// Add anti-cache in url if needed
			if ( s.cache === false ) {
				s.url = rts.test( cacheURL ) ?

					// If there is already a '_' parameter, set its value
					cacheURL.replace( rts, "$1_=" + nonce++ ) :

					// Otherwise add one to the end
					cacheURL + ( rquery.test( cacheURL ) ? "&" : "?" ) + "_=" + nonce++;
			}
		}

		// Set the If-Modified-Since and/or If-None-Match header, if in ifModified mode.
		if ( s.ifModified ) {
			if ( jQuery.lastModified[ cacheURL ] ) {
				jqXHR.setRequestHeader( "If-Modified-Since", jQuery.lastModified[ cacheURL ] );
			}
			if ( jQuery.etag[ cacheURL ] ) {
				jqXHR.setRequestHeader( "If-None-Match", jQuery.etag[ cacheURL ] );
			}
		}

		// Set the correct header, if data is being sent
		if ( s.data && s.hasContent && s.contentType !== false || options.contentType ) {
			jqXHR.setRequestHeader( "Content-Type", s.contentType );
		}

		// Set the Accepts header for the server, depending on the dataType
		jqXHR.setRequestHeader(
			"Accept",
			s.dataTypes[ 0 ] && s.accepts[ s.dataTypes[ 0 ] ] ?
				s.accepts[ s.dataTypes[ 0 ] ] +
					( s.dataTypes[ 0 ] !== "*" ? ", " + allTypes + "; q=0.01" : "" ) :
				s.accepts[ "*" ]
		);

		// Check for headers option
		for ( i in s.headers ) {
			jqXHR.setRequestHeader( i, s.headers[ i ] );
		}

		// Allow custom headers/mimetypes and early abort
		if ( s.beforeSend &&
			( s.beforeSend.call( callbackContext, jqXHR, s ) === false || state === 2 ) ) {

			// Abort if not done already and return
			return jqXHR.abort();
		}

		// aborting is no longer a cancellation
		strAbort = "abort";

		// Install callbacks on deferreds
		for ( i in { success: 1, error: 1, complete: 1 } ) {
			jqXHR[ i ]( s[ i ] );
		}

		// Get transport
		transport = inspectPrefiltersOrTransports( transports, s, options, jqXHR );

		// If no transport, we auto-abort
		if ( !transport ) {
			done( -1, "No Transport" );
		} else {
			jqXHR.readyState = 1;

			// Send global event
			if ( fireGlobals ) {
				globalEventContext.trigger( "ajaxSend", [ jqXHR, s ] );
			}

			// If request was aborted inside ajaxSend, stop there
			if ( state === 2 ) {
				return jqXHR;
			}

			// Timeout
			if ( s.async && s.timeout > 0 ) {
				timeoutTimer = window.setTimeout( function() {
					jqXHR.abort( "timeout" );
				}, s.timeout );
			}

			try {
				state = 1;
				transport.send( requestHeaders, done );
			} catch ( e ) {

				// Propagate exception as error if not done
				if ( state < 2 ) {
					done( -1, e );

				// Simply rethrow otherwise
				} else {
					throw e;
				}
			}
		}

		// Callback for when everything is done
		function done( status, nativeStatusText, responses, headers ) {
			var isSuccess, success, error, response, modified,
				statusText = nativeStatusText;

			// Called once
			if ( state === 2 ) {
				return;
			}

			// State is "done" now
			state = 2;

			// Clear timeout if it exists
			if ( timeoutTimer ) {
				window.clearTimeout( timeoutTimer );
			}

			// Dereference transport for early garbage collection
			// (no matter how long the jqXHR object will be used)
			transport = undefined;

			// Cache response headers
			responseHeadersString = headers || "";

			// Set readyState
			jqXHR.readyState = status > 0 ? 4 : 0;

			// Determine if successful
			isSuccess = status >= 200 && status < 300 || status === 304;

			// Get response data
			if ( responses ) {
				response = ajaxHandleResponses( s, jqXHR, responses );
			}

			// Convert no matter what (that way responseXXX fields are always set)
			response = ajaxConvert( s, response, jqXHR, isSuccess );

			// If successful, handle type chaining
			if ( isSuccess ) {

				// Set the If-Modified-Since and/or If-None-Match header, if in ifModified mode.
				if ( s.ifModified ) {
					modified = jqXHR.getResponseHeader( "Last-Modified" );
					if ( modified ) {
						jQuery.lastModified[ cacheURL ] = modified;
					}
					modified = jqXHR.getResponseHeader( "etag" );
					if ( modified ) {
						jQuery.etag[ cacheURL ] = modified;
					}
				}

				// if no content
				if ( status === 204 || s.type === "HEAD" ) {
					statusText = "nocontent";

				// if not modified
				} else if ( status === 304 ) {
					statusText = "notmodified";

				// If we have data, let's convert it
				} else {
					statusText = response.state;
					success = response.data;
					error = response.error;
					isSuccess = !error;
				}
			} else {

				// We extract error from statusText
				// then normalize statusText and status for non-aborts
				error = statusText;
				if ( status || !statusText ) {
					statusText = "error";
					if ( status < 0 ) {
						status = 0;
					}
				}
			}

			// Set data for the fake xhr object
			jqXHR.status = status;
			jqXHR.statusText = ( nativeStatusText || statusText ) + "";

			// Success/Error
			if ( isSuccess ) {
				deferred.resolveWith( callbackContext, [ success, statusText, jqXHR ] );
			} else {
				deferred.rejectWith( callbackContext, [ jqXHR, statusText, error ] );
			}

			// Status-dependent callbacks
			jqXHR.statusCode( statusCode );
			statusCode = undefined;

			if ( fireGlobals ) {
				globalEventContext.trigger( isSuccess ? "ajaxSuccess" : "ajaxError",
					[ jqXHR, s, isSuccess ? success : error ] );
			}

			// Complete
			completeDeferred.fireWith( callbackContext, [ jqXHR, statusText ] );

			if ( fireGlobals ) {
				globalEventContext.trigger( "ajaxComplete", [ jqXHR, s ] );

				// Handle the global AJAX counter
				if ( !( --jQuery.active ) ) {
					jQuery.event.trigger( "ajaxStop" );
				}
			}
		}

		return jqXHR;
	},

	getJSON: function( url, data, callback ) {
		return jQuery.get( url, data, callback, "json" );
	},

	getScript: function( url, callback ) {
		return jQuery.get( url, undefined, callback, "script" );
	}
} );

jQuery.each( [ "get", "post" ], function( i, method ) {
	jQuery[ method ] = function( url, data, callback, type ) {

		// shift arguments if data argument was omitted
		if ( jQuery.isFunction( data ) ) {
			type = type || callback;
			callback = data;
			data = undefined;
		}

		// The url can be an options object (which then must have .url)
		return jQuery.ajax( jQuery.extend( {
			url: url,
			type: method,
			dataType: type,
			data: data,
			success: callback
		}, jQuery.isPlainObject( url ) && url ) );
	};
} );


jQuery._evalUrl = function( url ) {
	return jQuery.ajax( {
		url: url,

		// Make this explicit, since user can override this through ajaxSetup (#11264)
		type: "GET",
		dataType: "script",
		cache: true,
		async: false,
		global: false,
		"throws": true
	} );
};


jQuery.fn.extend( {
	wrapAll: function( html ) {
		if ( jQuery.isFunction( html ) ) {
			return this.each( function( i ) {
				jQuery( this ).wrapAll( html.call( this, i ) );
			} );
		}

		if ( this[ 0 ] ) {

			// The elements to wrap the target around
			var wrap = jQuery( html, this[ 0 ].ownerDocument ).eq( 0 ).clone( true );

			if ( this[ 0 ].parentNode ) {
				wrap.insertBefore( this[ 0 ] );
			}

			wrap.map( function() {
				var elem = this;

				while ( elem.firstChild && elem.firstChild.nodeType === 1 ) {
					elem = elem.firstChild;
				}

				return elem;
			} ).append( this );
		}

		return this;
	},

	wrapInner: function( html ) {
		if ( jQuery.isFunction( html ) ) {
			return this.each( function( i ) {
				jQuery( this ).wrapInner( html.call( this, i ) );
			} );
		}

		return this.each( function() {
			var self = jQuery( this ),
				contents = self.contents();

			if ( contents.length ) {
				contents.wrapAll( html );

			} else {
				self.append( html );
			}
		} );
	},

	wrap: function( html ) {
		var isFunction = jQuery.isFunction( html );

		return this.each( function( i ) {
			jQuery( this ).wrapAll( isFunction ? html.call( this, i ) : html );
		} );
	},

	unwrap: function() {
		return this.parent().each( function() {
			if ( !jQuery.nodeName( this, "body" ) ) {
				jQuery( this ).replaceWith( this.childNodes );
			}
		} ).end();
	}
} );


function getDisplay( elem ) {
	return elem.style && elem.style.display || jQuery.css( elem, "display" );
}

function filterHidden( elem ) {
	while ( elem && elem.nodeType === 1 ) {
		if ( getDisplay( elem ) === "none" || elem.type === "hidden" ) {
			return true;
		}
		elem = elem.parentNode;
	}
	return false;
}

jQuery.expr.filters.hidden = function( elem ) {

	// Support: Opera <= 12.12
	// Opera reports offsetWidths and offsetHeights less than zero on some elements
	return support.reliableHiddenOffsets() ?
		( elem.offsetWidth <= 0 && elem.offsetHeight <= 0 &&
			!elem.getClientRects().length ) :
			filterHidden( elem );
};

jQuery.expr.filters.visible = function( elem ) {
	return !jQuery.expr.filters.hidden( elem );
};




var r20 = /%20/g,
	rbracket = /\[\]$/,
	rCRLF = /\r?\n/g,
	rsubmitterTypes = /^(?:submit|button|image|reset|file)$/i,
	rsubmittable = /^(?:input|select|textarea|keygen)/i;

function buildParams( prefix, obj, traditional, add ) {
	var name;

	if ( jQuery.isArray( obj ) ) {

		// Serialize array item.
		jQuery.each( obj, function( i, v ) {
			if ( traditional || rbracket.test( prefix ) ) {

				// Treat each array item as a scalar.
				add( prefix, v );

			} else {

				// Item is non-scalar (array or object), encode its numeric index.
				buildParams(
					prefix + "[" + ( typeof v === "object" && v != null ? i : "" ) + "]",
					v,
					traditional,
					add
				);
			}
		} );

	} else if ( !traditional && jQuery.type( obj ) === "object" ) {

		// Serialize object item.
		for ( name in obj ) {
			buildParams( prefix + "[" + name + "]", obj[ name ], traditional, add );
		}

	} else {

		// Serialize scalar item.
		add( prefix, obj );
	}
}

// Serialize an array of form elements or a set of
// key/values into a query string
jQuery.param = function( a, traditional ) {
	var prefix,
		s = [],
		add = function( key, value ) {

			// If value is a function, invoke it and return its value
			value = jQuery.isFunction( value ) ? value() : ( value == null ? "" : value );
			s[ s.length ] = encodeURIComponent( key ) + "=" + encodeURIComponent( value );
		};

	// Set traditional to true for jQuery <= 1.3.2 behavior.
	if ( traditional === undefined ) {
		traditional = jQuery.ajaxSettings && jQuery.ajaxSettings.traditional;
	}

	// If an array was passed in, assume that it is an array of form elements.
	if ( jQuery.isArray( a ) || ( a.jquery && !jQuery.isPlainObject( a ) ) ) {

		// Serialize the form elements
		jQuery.each( a, function() {
			add( this.name, this.value );
		} );

	} else {

		// If traditional, encode the "old" way (the way 1.3.2 or older
		// did it), otherwise encode params recursively.
		for ( prefix in a ) {
			buildParams( prefix, a[ prefix ], traditional, add );
		}
	}

	// Return the resulting serialization
	return s.join( "&" ).replace( r20, "+" );
};

jQuery.fn.extend( {
	serialize: function() {
		return jQuery.param( this.serializeArray() );
	},
	serializeArray: function() {
		return this.map( function() {

			// Can add propHook for "elements" to filter or add form elements
			var elements = jQuery.prop( this, "elements" );
			return elements ? jQuery.makeArray( elements ) : this;
		} )
		.filter( function() {
			var type = this.type;

			// Use .is(":disabled") so that fieldset[disabled] works
			return this.name && !jQuery( this ).is( ":disabled" ) &&
				rsubmittable.test( this.nodeName ) && !rsubmitterTypes.test( type ) &&
				( this.checked || !rcheckableType.test( type ) );
		} )
		.map( function( i, elem ) {
			var val = jQuery( this ).val();

			return val == null ?
				null :
				jQuery.isArray( val ) ?
					jQuery.map( val, function( val ) {
						return { name: elem.name, value: val.replace( rCRLF, "\r\n" ) };
					} ) :
					{ name: elem.name, value: val.replace( rCRLF, "\r\n" ) };
		} ).get();
	}
} );


// Create the request object
// (This is still attached to ajaxSettings for backward compatibility)
jQuery.ajaxSettings.xhr = window.ActiveXObject !== undefined ?

	// Support: IE6-IE8
	function() {

		// XHR cannot access local files, always use ActiveX for that case
		if ( this.isLocal ) {
			return createActiveXHR();
		}

		// Support: IE 9-11
		// IE seems to error on cross-domain PATCH requests when ActiveX XHR
		// is used. In IE 9+ always use the native XHR.
		// Note: this condition won't catch Edge as it doesn't define
		// document.documentMode but it also doesn't support ActiveX so it won't
		// reach this code.
		if ( document.documentMode > 8 ) {
			return createStandardXHR();
		}

		// Support: IE<9
		// oldIE XHR does not support non-RFC2616 methods (#13240)
		// See http://msdn.microsoft.com/en-us/library/ie/ms536648(v=vs.85).aspx
		// and http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9
		// Although this check for six methods instead of eight
		// since IE also does not support "trace" and "connect"
		return /^(get|post|head|put|delete|options)$/i.test( this.type ) &&
			createStandardXHR() || createActiveXHR();
	} :

	// For all other browsers, use the standard XMLHttpRequest object
	createStandardXHR;

var xhrId = 0,
	xhrCallbacks = {},
	xhrSupported = jQuery.ajaxSettings.xhr();

// Support: IE<10
// Open requests must be manually aborted on unload (#5280)
// See https://support.microsoft.com/kb/2856746 for more info
if ( window.attachEvent ) {
	window.attachEvent( "onunload", function() {
		for ( var key in xhrCallbacks ) {
			xhrCallbacks[ key ]( undefined, true );
		}
	} );
}

// Determine support properties
support.cors = !!xhrSupported && ( "withCredentials" in xhrSupported );
xhrSupported = support.ajax = !!xhrSupported;

// Create transport if the browser can provide an xhr
if ( xhrSupported ) {

	jQuery.ajaxTransport( function( options ) {

		// Cross domain only allowed if supported through XMLHttpRequest
		if ( !options.crossDomain || support.cors ) {

			var callback;

			return {
				send: function( headers, complete ) {
					var i,
						xhr = options.xhr(),
						id = ++xhrId;

					// Open the socket
					xhr.open(
						options.type,
						options.url,
						options.async,
						options.username,
						options.password
					);

					// Apply custom fields if provided
					if ( options.xhrFields ) {
						for ( i in options.xhrFields ) {
							xhr[ i ] = options.xhrFields[ i ];
						}
					}

					// Override mime type if needed
					if ( options.mimeType && xhr.overrideMimeType ) {
						xhr.overrideMimeType( options.mimeType );
					}

					// X-Requested-With header
					// For cross-domain requests, seeing as conditions for a preflight are
					// akin to a jigsaw puzzle, we simply never set it to be sure.
					// (it can always be set on a per-request basis or even using ajaxSetup)
					// For same-domain requests, won't change header if already provided.
					if ( !options.crossDomain && !headers[ "X-Requested-With" ] ) {
						headers[ "X-Requested-With" ] = "XMLHttpRequest";
					}

					// Set headers
					for ( i in headers ) {

						// Support: IE<9
						// IE's ActiveXObject throws a 'Type Mismatch' exception when setting
						// request header to a null-value.
						//
						// To keep consistent with other XHR implementations, cast the value
						// to string and ignore `undefined`.
						if ( headers[ i ] !== undefined ) {
							xhr.setRequestHeader( i, headers[ i ] + "" );
						}
					}

					// Do send the request
					// This may raise an exception which is actually
					// handled in jQuery.ajax (so no try/catch here)
					xhr.send( ( options.hasContent && options.data ) || null );

					// Listener
					callback = function( _, isAbort ) {
						var status, statusText, responses;

						// Was never called and is aborted or complete
						if ( callback && ( isAbort || xhr.readyState === 4 ) ) {

							// Clean up
							delete xhrCallbacks[ id ];
							callback = undefined;
							xhr.onreadystatechange = jQuery.noop;

							// Abort manually if needed
							if ( isAbort ) {
								if ( xhr.readyState !== 4 ) {
									xhr.abort();
								}
							} else {
								responses = {};
								status = xhr.status;

								// Support: IE<10
								// Accessing binary-data responseText throws an exception
								// (#11426)
								if ( typeof xhr.responseText === "string" ) {
									responses.text = xhr.responseText;
								}

								// Firefox throws an exception when accessing
								// statusText for faulty cross-domain requests
								try {
									statusText = xhr.statusText;
								} catch ( e ) {

									// We normalize with Webkit giving an empty statusText
									statusText = "";
								}

								// Filter status for non standard behaviors

								// If the request is local and we have data: assume a success
								// (success with no data won't get notified, that's the best we
								// can do given current implementations)
								if ( !status && options.isLocal && !options.crossDomain ) {
									status = responses.text ? 200 : 404;

								// IE - #1450: sometimes returns 1223 when it should be 204
								} else if ( status === 1223 ) {
									status = 204;
								}
							}
						}

						// Call complete if needed
						if ( responses ) {
							complete( status, statusText, responses, xhr.getAllResponseHeaders() );
						}
					};

					// Do send the request
					// `xhr.send` may raise an exception, but it will be
					// handled in jQuery.ajax (so no try/catch here)
					if ( !options.async ) {

						// If we're in sync mode we fire the callback
						callback();
					} else if ( xhr.readyState === 4 ) {

						// (IE6 & IE7) if it's in cache and has been
						// retrieved directly we need to fire the callback
						window.setTimeout( callback );
					} else {

						// Register the callback, but delay it in case `xhr.send` throws
						// Add to the list of active xhr callbacks
						xhr.onreadystatechange = xhrCallbacks[ id ] = callback;
					}
				},

				abort: function() {
					if ( callback ) {
						callback( undefined, true );
					}
				}
			};
		}
	} );
}

// Functions to create xhrs
function createStandardXHR() {
	try {
		return new window.XMLHttpRequest();
	} catch ( e ) {}
}

function createActiveXHR() {
	try {
		return new window.ActiveXObject( "Microsoft.XMLHTTP" );
	} catch ( e ) {}
}




// Prevent auto-execution of scripts when no explicit dataType was provided (See gh-2432)
jQuery.ajaxPrefilter( function( s ) {
	if ( s.crossDomain ) {
		s.contents.script = false;
	}
} );

// Install script dataType
jQuery.ajaxSetup( {
	accepts: {
		script: "text/javascript, application/javascript, " +
			"application/ecmascript, application/x-ecmascript"
	},
	contents: {
		script: /\b(?:java|ecma)script\b/
	},
	converters: {
		"text script": function( text ) {
			jQuery.globalEval( text );
			return text;
		}
	}
} );

// Handle cache's special case and global
jQuery.ajaxPrefilter( "script", function( s ) {
	if ( s.cache === undefined ) {
		s.cache = false;
	}
	if ( s.crossDomain ) {
		s.type = "GET";
		s.global = false;
	}
} );

// Bind script tag hack transport
jQuery.ajaxTransport( "script", function( s ) {

	// This transport only deals with cross domain requests
	if ( s.crossDomain ) {

		var script,
			head = document.head || jQuery( "head" )[ 0 ] || document.documentElement;

		return {

			send: function( _, callback ) {

				script = document.createElement( "script" );

				script.async = true;

				if ( s.scriptCharset ) {
					script.charset = s.scriptCharset;
				}

				script.src = s.url;

				// Attach handlers for all browsers
				script.onload = script.onreadystatechange = function( _, isAbort ) {

					if ( isAbort || !script.readyState || /loaded|complete/.test( script.readyState ) ) {

						// Handle memory leak in IE
						script.onload = script.onreadystatechange = null;

						// Remove the script
						if ( script.parentNode ) {
							script.parentNode.removeChild( script );
						}

						// Dereference the script
						script = null;

						// Callback if not abort
						if ( !isAbort ) {
							callback( 200, "success" );
						}
					}
				};

				// Circumvent IE6 bugs with base elements (#2709 and #4378) by prepending
				// Use native DOM manipulation to avoid our domManip AJAX trickery
				head.insertBefore( script, head.firstChild );
			},

			abort: function() {
				if ( script ) {
					script.onload( undefined, true );
				}
			}
		};
	}
} );




var oldCallbacks = [],
	rjsonp = /(=)\?(?=&|$)|\?\?/;

// Default jsonp settings
jQuery.ajaxSetup( {
	jsonp: "callback",
	jsonpCallback: function() {
		var callback = oldCallbacks.pop() || ( jQuery.expando + "_" + ( nonce++ ) );
		this[ callback ] = true;
		return callback;
	}
} );

// Detect, normalize options and install callbacks for jsonp requests
jQuery.ajaxPrefilter( "json jsonp", function( s, originalSettings, jqXHR ) {

	var callbackName, overwritten, responseContainer,
		jsonProp = s.jsonp !== false && ( rjsonp.test( s.url ) ?
			"url" :
			typeof s.data === "string" &&
				( s.contentType || "" )
					.indexOf( "application/x-www-form-urlencoded" ) === 0 &&
				rjsonp.test( s.data ) && "data"
		);

	// Handle iff the expected data type is "jsonp" or we have a parameter to set
	if ( jsonProp || s.dataTypes[ 0 ] === "jsonp" ) {

		// Get callback name, remembering preexisting value associated with it
		callbackName = s.jsonpCallback = jQuery.isFunction( s.jsonpCallback ) ?
			s.jsonpCallback() :
			s.jsonpCallback;

		// Insert callback into url or form data
		if ( jsonProp ) {
			s[ jsonProp ] = s[ jsonProp ].replace( rjsonp, "$1" + callbackName );
		} else if ( s.jsonp !== false ) {
			s.url += ( rquery.test( s.url ) ? "&" : "?" ) + s.jsonp + "=" + callbackName;
		}

		// Use data converter to retrieve json after script execution
		s.converters[ "script json" ] = function() {
			if ( !responseContainer ) {
				jQuery.error( callbackName + " was not called" );
			}
			return responseContainer[ 0 ];
		};

		// force json dataType
		s.dataTypes[ 0 ] = "json";

		// Install callback
		overwritten = window[ callbackName ];
		window[ callbackName ] = function() {
			responseContainer = arguments;
		};

		// Clean-up function (fires after converters)
		jqXHR.always( function() {

			// If previous value didn't exist - remove it
			if ( overwritten === undefined ) {
				jQuery( window ).removeProp( callbackName );

			// Otherwise restore preexisting value
			} else {
				window[ callbackName ] = overwritten;
			}

			// Save back as free
			if ( s[ callbackName ] ) {

				// make sure that re-using the options doesn't screw things around
				s.jsonpCallback = originalSettings.jsonpCallback;

				// save the callback name for future use
				oldCallbacks.push( callbackName );
			}

			// Call if it was a function and we have a response
			if ( responseContainer && jQuery.isFunction( overwritten ) ) {
				overwritten( responseContainer[ 0 ] );
			}

			responseContainer = overwritten = undefined;
		} );

		// Delegate to script
		return "script";
	}
} );




// Support: Safari 8+
// In Safari 8 documents created via document.implementation.createHTMLDocument
// collapse sibling forms: the second one becomes a child of the first one.
// Because of that, this security measure has to be disabled in Safari 8.
// https://bugs.webkit.org/show_bug.cgi?id=137337
support.createHTMLDocument = ( function() {
	if ( !document.implementation.createHTMLDocument ) {
		return false;
	}
	var doc = document.implementation.createHTMLDocument( "" );
	doc.body.innerHTML = "<form></form><form></form>";
	return doc.body.childNodes.length === 2;
} )();


// data: string of html
// context (optional): If specified, the fragment will be created in this context,
// defaults to document
// keepScripts (optional): If true, will include scripts passed in the html string
jQuery.parseHTML = function( data, context, keepScripts ) {
	if ( !data || typeof data !== "string" ) {
		return null;
	}
	if ( typeof context === "boolean" ) {
		keepScripts = context;
		context = false;
	}

	// document.implementation stops scripts or inline event handlers from
	// being executed immediately
	context = context || ( support.createHTMLDocument ?
		document.implementation.createHTMLDocument( "" ) :
		document );

	var parsed = rsingleTag.exec( data ),
		scripts = !keepScripts && [];

	// Single tag
	if ( parsed ) {
		return [ context.createElement( parsed[ 1 ] ) ];
	}

	parsed = buildFragment( [ data ], context, scripts );

	if ( scripts && scripts.length ) {
		jQuery( scripts ).remove();
	}

	return jQuery.merge( [], parsed.childNodes );
};


// Keep a copy of the old load method
var _load = jQuery.fn.load;

/**
 * Load a url into a page
 */
jQuery.fn.load = function( url, params, callback ) {
	if ( typeof url !== "string" && _load ) {
		return _load.apply( this, arguments );
	}

	var selector, type, response,
		self = this,
		off = url.indexOf( " " );

	if ( off > -1 ) {
		selector = jQuery.trim( url.slice( off, url.length ) );
		url = url.slice( 0, off );
	}

	// If it's a function
	if ( jQuery.isFunction( params ) ) {

		// We assume that it's the callback
		callback = params;
		params = undefined;

	// Otherwise, build a param string
	} else if ( params && typeof params === "object" ) {
		type = "POST";
	}

	// If we have elements to modify, make the request
	if ( self.length > 0 ) {
		jQuery.ajax( {
			url: url,

			// If "type" variable is undefined, then "GET" method will be used.
			// Make value of this field explicit since
			// user can override it through ajaxSetup method
			type: type || "GET",
			dataType: "html",
			data: params
		} ).done( function( responseText ) {

			// Save response for use in complete callback
			response = arguments;

			self.html( selector ?

				// If a selector was specified, locate the right elements in a dummy div
				// Exclude scripts to avoid IE 'Permission Denied' errors
				jQuery( "<div>" ).append( jQuery.parseHTML( responseText ) ).find( selector ) :

				// Otherwise use the full result
				responseText );

		// If the request succeeds, this function gets "data", "status", "jqXHR"
		// but they are ignored because response was set above.
		// If it fails, this function gets "jqXHR", "status", "error"
		} ).always( callback && function( jqXHR, status ) {
			self.each( function() {
				callback.apply( self, response || [ jqXHR.responseText, status, jqXHR ] );
			} );
		} );
	}

	return this;
};




// Attach a bunch of functions for handling common AJAX events
jQuery.each( [
	"ajaxStart",
	"ajaxStop",
	"ajaxComplete",
	"ajaxError",
	"ajaxSuccess",
	"ajaxSend"
], function( i, type ) {
	jQuery.fn[ type ] = function( fn ) {
		return this.on( type, fn );
	};
} );




jQuery.expr.filters.animated = function( elem ) {
	return jQuery.grep( jQuery.timers, function( fn ) {
		return elem === fn.elem;
	} ).length;
};





/**
 * Gets a window from an element
 */
function getWindow( elem ) {
	return jQuery.isWindow( elem ) ?
		elem :
		elem.nodeType === 9 ?
			elem.defaultView || elem.parentWindow :
			false;
}

jQuery.offset = {
	setOffset: function( elem, options, i ) {
		var curPosition, curLeft, curCSSTop, curTop, curOffset, curCSSLeft, calculatePosition,
			position = jQuery.css( elem, "position" ),
			curElem = jQuery( elem ),
			props = {};

		// set position first, in-case top/left are set even on static elem
		if ( position === "static" ) {
			elem.style.position = "relative";
		}

		curOffset = curElem.offset();
		curCSSTop = jQuery.css( elem, "top" );
		curCSSLeft = jQuery.css( elem, "left" );
		calculatePosition = ( position === "absolute" || position === "fixed" ) &&
			jQuery.inArray( "auto", [ curCSSTop, curCSSLeft ] ) > -1;

		// need to be able to calculate position if either top or left
		// is auto and position is either absolute or fixed
		if ( calculatePosition ) {
			curPosition = curElem.position();
			curTop = curPosition.top;
			curLeft = curPosition.left;
		} else {
			curTop = parseFloat( curCSSTop ) || 0;
			curLeft = parseFloat( curCSSLeft ) || 0;
		}

		if ( jQuery.isFunction( options ) ) {

			// Use jQuery.extend here to allow modification of coordinates argument (gh-1848)
			options = options.call( elem, i, jQuery.extend( {}, curOffset ) );
		}

		if ( options.top != null ) {
			props.top = ( options.top - curOffset.top ) + curTop;
		}
		if ( options.left != null ) {
			props.left = ( options.left - curOffset.left ) + curLeft;
		}

		if ( "using" in options ) {
			options.using.call( elem, props );
		} else {
			curElem.css( props );
		}
	}
};

jQuery.fn.extend( {
	offset: function( options ) {
		if ( arguments.length ) {
			return options === undefined ?
				this :
				this.each( function( i ) {
					jQuery.offset.setOffset( this, options, i );
				} );
		}

		var docElem, win,
			box = { top: 0, left: 0 },
			elem = this[ 0 ],
			doc = elem && elem.ownerDocument;

		if ( !doc ) {
			return;
		}

		docElem = doc.documentElement;

		// Make sure it's not a disconnected DOM node
		if ( !jQuery.contains( docElem, elem ) ) {
			return box;
		}

		// If we don't have gBCR, just use 0,0 rather than error
		// BlackBerry 5, iOS 3 (original iPhone)
		if ( typeof elem.getBoundingClientRect !== "undefined" ) {
			box = elem.getBoundingClientRect();
		}
		win = getWindow( doc );
		return {
			top: box.top  + ( win.pageYOffset || docElem.scrollTop )  - ( docElem.clientTop  || 0 ),
			left: box.left + ( win.pageXOffset || docElem.scrollLeft ) - ( docElem.clientLeft || 0 )
		};
	},

	position: function() {
		if ( !this[ 0 ] ) {
			return;
		}

		var offsetParent, offset,
			parentOffset = { top: 0, left: 0 },
			elem = this[ 0 ];

		// Fixed elements are offset from window (parentOffset = {top:0, left: 0},
		// because it is its only offset parent
		if ( jQuery.css( elem, "position" ) === "fixed" ) {

			// we assume that getBoundingClientRect is available when computed position is fixed
			offset = elem.getBoundingClientRect();
		} else {

			// Get *real* offsetParent
			offsetParent = this.offsetParent();

			// Get correct offsets
			offset = this.offset();
			if ( !jQuery.nodeName( offsetParent[ 0 ], "html" ) ) {
				parentOffset = offsetParent.offset();
			}

			// Add offsetParent borders
			// Subtract offsetParent scroll positions
			parentOffset.top += jQuery.css( offsetParent[ 0 ], "borderTopWidth", true ) -
				offsetParent.scrollTop();
			parentOffset.left += jQuery.css( offsetParent[ 0 ], "borderLeftWidth", true ) -
				offsetParent.scrollLeft();
		}

		// Subtract parent offsets and element margins
		// note: when an element has margin: auto the offsetLeft and marginLeft
		// are the same in Safari causing offset.left to incorrectly be 0
		return {
			top:  offset.top  - parentOffset.top - jQuery.css( elem, "marginTop", true ),
			left: offset.left - parentOffset.left - jQuery.css( elem, "marginLeft", true )
		};
	},

	offsetParent: function() {
		return this.map( function() {
			var offsetParent = this.offsetParent;

			while ( offsetParent && ( !jQuery.nodeName( offsetParent, "html" ) &&
				jQuery.css( offsetParent, "position" ) === "static" ) ) {
				offsetParent = offsetParent.offsetParent;
			}
			return offsetParent || documentElement;
		} );
	}
} );

// Create scrollLeft and scrollTop methods
jQuery.each( { scrollLeft: "pageXOffset", scrollTop: "pageYOffset" }, function( method, prop ) {
	var top = /Y/.test( prop );

	jQuery.fn[ method ] = function( val ) {
		return access( this, function( elem, method, val ) {
			var win = getWindow( elem );

			if ( val === undefined ) {
				return win ? ( prop in win ) ? win[ prop ] :
					win.document.documentElement[ method ] :
					elem[ method ];
			}

			if ( win ) {
				win.scrollTo(
					!top ? val : jQuery( win ).scrollLeft(),
					top ? val : jQuery( win ).scrollTop()
				);

			} else {
				elem[ method ] = val;
			}
		}, method, val, arguments.length, null );
	};
} );

// Support: Safari<7-8+, Chrome<37-44+
// Add the top/left cssHooks using jQuery.fn.position
// Webkit bug: https://bugs.webkit.org/show_bug.cgi?id=29084
// getComputedStyle returns percent when specified for top/left/bottom/right
// rather than make the css module depend on the offset module, we just check for it here
jQuery.each( [ "top", "left" ], function( i, prop ) {
	jQuery.cssHooks[ prop ] = addGetHookIf( support.pixelPosition,
		function( elem, computed ) {
			if ( computed ) {
				computed = curCSS( elem, prop );

				// if curCSS returns percentage, fallback to offset
				return rnumnonpx.test( computed ) ?
					jQuery( elem ).position()[ prop ] + "px" :
					computed;
			}
		}
	);
} );


// Create innerHeight, innerWidth, height, width, outerHeight and outerWidth methods
jQuery.each( { Height: "height", Width: "width" }, function( name, type ) {
	jQuery.each( { padding: "inner" + name, content: type, "": "outer" + name },
	function( defaultExtra, funcName ) {

		// margin is only for outerHeight, outerWidth
		jQuery.fn[ funcName ] = function( margin, value ) {
			var chainable = arguments.length && ( defaultExtra || typeof margin !== "boolean" ),
				extra = defaultExtra || ( margin === true || value === true ? "margin" : "border" );

			return access( this, function( elem, type, value ) {
				var doc;

				if ( jQuery.isWindow( elem ) ) {

					// As of 5/8/2012 this will yield incorrect results for Mobile Safari, but there
					// isn't a whole lot we can do. See pull request at this URL for discussion:
					// https://github.com/jquery/jquery/pull/764
					return elem.document.documentElement[ "client" + name ];
				}

				// Get document width or height
				if ( elem.nodeType === 9 ) {
					doc = elem.documentElement;

					// Either scroll[Width/Height] or offset[Width/Height] or client[Width/Height],
					// whichever is greatest
					// unfortunately, this causes bug #3838 in IE6/8 only,
					// but there is currently no good, small way to fix it.
					return Math.max(
						elem.body[ "scroll" + name ], doc[ "scroll" + name ],
						elem.body[ "offset" + name ], doc[ "offset" + name ],
						doc[ "client" + name ]
					);
				}

				return value === undefined ?

					// Get width or height on the element, requesting but not forcing parseFloat
					jQuery.css( elem, type, extra ) :

					// Set width or height on the element
					jQuery.style( elem, type, value, extra );
			}, type, chainable ? margin : undefined, chainable, null );
		};
	} );
} );


jQuery.fn.extend( {

	bind: function( types, data, fn ) {
		return this.on( types, null, data, fn );
	},
	unbind: function( types, fn ) {
		return this.off( types, null, fn );
	},

	delegate: function( selector, types, data, fn ) {
		return this.on( types, selector, data, fn );
	},
	undelegate: function( selector, types, fn ) {

		// ( namespace ) or ( selector, types [, fn] )
		return arguments.length === 1 ?
			this.off( selector, "**" ) :
			this.off( types, selector || "**", fn );
	}
} );

// The number of elements contained in the matched element set
jQuery.fn.size = function() {
	return this.length;
};

jQuery.fn.andSelf = jQuery.fn.addBack;




// Register as a named AMD module, since jQuery can be concatenated with other
// files that may use define, but not via a proper concatenation script that
// understands anonymous AMD modules. A named AMD is safest and most robust
// way to register. Lowercase jquery is used because AMD module names are
// derived from file names, and jQuery is normally delivered in a lowercase
// file name. Do this after creating the global so that if an AMD module wants
// to call noConflict to hide this version of jQuery, it will work.

// Note that for maximum portability, libraries that are not jQuery should
// declare themselves as anonymous modules, and avoid setting a global if an
// AMD loader is present. jQuery is a special case. For more information, see
// https://github.com/jrburke/requirejs/wiki/Updating-existing-libraries#wiki-anon

if ( typeof define === "function" && define.amd ) {
	define( "jquery", [], function() {
		return jQuery;
	} );
}



var

	// Map over jQuery in case of overwrite
	_jQuery = window.jQuery,

	// Map over the $ in case of overwrite
	_$ = window.$;

jQuery.noConflict = function( deep ) {
	if ( window.$ === jQuery ) {
		window.$ = _$;
	}

	if ( deep && window.jQuery === jQuery ) {
		window.jQuery = _jQuery;
	}

	return jQuery;
};

// Expose jQuery and $ identifiers, even in
// AMD (#7102#comment:10, https://github.com/jquery/jquery/pull/557)
// and CommonJS for browser emulators (#13566)
if ( !noGlobal ) {
	window.jQuery = window.$ = jQuery;
}

return jQuery;
}));
(function($, undefined) {

/**
 * Unobtrusive scripting adapter for jQuery
 * https://github.com/rails/jquery-ujs
 *
 * Requires jQuery 1.8.0 or later.
 *
 * Released under the MIT license
 *
 */

  // Cut down on the number of issues from people inadvertently including jquery_ujs twice
  // by detecting and raising an error when it happens.
  'use strict';

  if ( $.rails !== undefined ) {
    $.error('jquery-ujs has already been loaded!');
  }

  // Shorthand to make it a little easier to call public rails functions from within rails.js
  var rails;
  var $document = $(document);

  $.rails = rails = {
    // Link elements bound by jquery-ujs
    linkClickSelector: 'a[data-confirm], a[data-method], a[data-remote]:not([disabled]), a[data-disable-with], a[data-disable]',

    // Button elements bound by jquery-ujs
    buttonClickSelector: 'button[data-remote]:not([form]):not(form button), button[data-confirm]:not([form]):not(form button)',

    // Select elements bound by jquery-ujs
    inputChangeSelector: 'select[data-remote], input[data-remote], textarea[data-remote]',

    // Form elements bound by jquery-ujs
    formSubmitSelector: 'form',

    // Form input elements bound by jquery-ujs
    formInputClickSelector: 'form input[type=submit], form input[type=image], form button[type=submit], form button:not([type]), input[type=submit][form], input[type=image][form], button[type=submit][form], button[form]:not([type])',

    // Form input elements disabled during form submission
    disableSelector: 'input[data-disable-with]:enabled, button[data-disable-with]:enabled, textarea[data-disable-with]:enabled, input[data-disable]:enabled, button[data-disable]:enabled, textarea[data-disable]:enabled',

    // Form input elements re-enabled after form submission
    enableSelector: 'input[data-disable-with]:disabled, button[data-disable-with]:disabled, textarea[data-disable-with]:disabled, input[data-disable]:disabled, button[data-disable]:disabled, textarea[data-disable]:disabled',

    // Form required input elements
    requiredInputSelector: 'input[name][required]:not([disabled]), textarea[name][required]:not([disabled])',

    // Form file input elements
    fileInputSelector: 'input[type=file]:not([disabled])',

    // Link onClick disable selector with possible reenable after remote submission
    linkDisableSelector: 'a[data-disable-with], a[data-disable]',

    // Button onClick disable selector with possible reenable after remote submission
    buttonDisableSelector: 'button[data-remote][data-disable-with], button[data-remote][data-disable]',

    // Up-to-date Cross-Site Request Forgery token
    csrfToken: function() {
     return $('meta[name=csrf-token]').attr('content');
    },

    // URL param that must contain the CSRF token
    csrfParam: function() {
     return $('meta[name=csrf-param]').attr('content');
    },

    // Make sure that every Ajax request sends the CSRF token
    CSRFProtection: function(xhr) {
      var token = rails.csrfToken();
      if (token) xhr.setRequestHeader('X-CSRF-Token', token);
    },

    // Make sure that all forms have actual up-to-date tokens (cached forms contain old ones)
    refreshCSRFTokens: function(){
      $('form input[name="' + rails.csrfParam() + '"]').val(rails.csrfToken());
    },

    // Triggers an event on an element and returns false if the event result is false
    fire: function(obj, name, data) {
      var event = $.Event(name);
      obj.trigger(event, data);
      return event.result !== false;
    },

    // Default confirm dialog, may be overridden with custom confirm dialog in $.rails.confirm
    confirm: function(message) {
      return confirm(message);
    },

    // Default ajax function, may be overridden with custom function in $.rails.ajax
    ajax: function(options) {
      return $.ajax(options);
    },

    // Default way to get an element's href. May be overridden at $.rails.href.
    href: function(element) {
      return element[0].href;
    },

    // Checks "data-remote" if true to handle the request through a XHR request.
    isRemote: function(element) {
      return element.data('remote') !== undefined && element.data('remote') !== false;
    },

    // Submits "remote" forms and links with ajax
    handleRemote: function(element) {
      var method, url, data, withCredentials, dataType, options;

      if (rails.fire(element, 'ajax:before')) {
        withCredentials = element.data('with-credentials') || null;
        dataType = element.data('type') || ($.ajaxSettings && $.ajaxSettings.dataType);

        if (element.is('form')) {
          method = element.data('ujs:submit-button-formmethod') || element.attr('method');
          url = element.data('ujs:submit-button-formaction') || element.attr('action');
          data = $(element[0].elements).serializeArray();
          // memoized value from clicked submit button
          var button = element.data('ujs:submit-button');
          if (button) {
            data.push(button);
            element.data('ujs:submit-button', null);
          }
          element.data('ujs:submit-button-formmethod', null);
          element.data('ujs:submit-button-formaction', null);
        } else if (element.is(rails.inputChangeSelector)) {
          method = element.data('method');
          url = element.data('url');
          data = element.serialize();
          if (element.data('params')) data = data + '&' + element.data('params');
        } else if (element.is(rails.buttonClickSelector)) {
          method = element.data('method') || 'get';
          url = element.data('url');
          data = element.serialize();
          if (element.data('params')) data = data + '&' + element.data('params');
        } else {
          method = element.data('method');
          url = rails.href(element);
          data = element.data('params') || null;
        }

        options = {
          type: method || 'GET', data: data, dataType: dataType,
          // stopping the "ajax:beforeSend" event will cancel the ajax request
          beforeSend: function(xhr, settings) {
            if (settings.dataType === undefined) {
              xhr.setRequestHeader('accept', '*/*;q=0.5, ' + settings.accepts.script);
            }
            if (rails.fire(element, 'ajax:beforeSend', [xhr, settings])) {
              element.trigger('ajax:send', xhr);
            } else {
              return false;
            }
          },
          success: function(data, status, xhr) {
            element.trigger('ajax:success', [data, status, xhr]);
          },
          complete: function(xhr, status) {
            element.trigger('ajax:complete', [xhr, status]);
          },
          error: function(xhr, status, error) {
            element.trigger('ajax:error', [xhr, status, error]);
          },
          crossDomain: rails.isCrossDomain(url)
        };

        // There is no withCredentials for IE6-8 when
        // "Enable native XMLHTTP support" is disabled
        if (withCredentials) {
          options.xhrFields = {
            withCredentials: withCredentials
          };
        }

        // Only pass url to `ajax` options if not blank
        if (url) { options.url = url; }

        return rails.ajax(options);
      } else {
        return false;
      }
    },

    // Determines if the request is a cross domain request.
    isCrossDomain: function(url) {
      var originAnchor = document.createElement('a');
      originAnchor.href = location.href;
      var urlAnchor = document.createElement('a');

      try {
        urlAnchor.href = url;
        // This is a workaround to a IE bug.
        urlAnchor.href = urlAnchor.href;

        // If URL protocol is false or is a string containing a single colon
        // *and* host are false, assume it is not a cross-domain request
        // (should only be the case for IE7 and IE compatibility mode).
        // Otherwise, evaluate protocol and host of the URL against the origin
        // protocol and host.
        return !(((!urlAnchor.protocol || urlAnchor.protocol === ':') && !urlAnchor.host) ||
          (originAnchor.protocol + '//' + originAnchor.host ===
            urlAnchor.protocol + '//' + urlAnchor.host));
      } catch (e) {
        // If there is an error parsing the URL, assume it is crossDomain.
        return true;
      }
    },

    // Handles "data-method" on links such as:
    // <a href="/users/5" data-method="delete" rel="nofollow" data-confirm="Are you sure?">Delete</a>
    handleMethod: function(link) {
      var href = rails.href(link),
        method = link.data('method'),
        target = link.attr('target'),
        csrfToken = rails.csrfToken(),
        csrfParam = rails.csrfParam(),
        form = $('<form method="post" action="' + href + '"></form>'),
        metadataInput = '<input name="_method" value="' + method + '" type="hidden" />';

      if (csrfParam !== undefined && csrfToken !== undefined && !rails.isCrossDomain(href)) {
        metadataInput += '<input name="' + csrfParam + '" value="' + csrfToken + '" type="hidden" />';
      }

      if (target) { form.attr('target', target); }

      form.hide().append(metadataInput).appendTo('body');
      form.submit();
    },

    // Helper function that returns form elements that match the specified CSS selector
    // If form is actually a "form" element this will return associated elements outside the from that have
    // the html form attribute set
    formElements: function(form, selector) {
      return form.is('form') ? $(form[0].elements).filter(selector) : form.find(selector);
    },

    /* Disables form elements:
      - Caches element value in 'ujs:enable-with' data store
      - Replaces element text with value of 'data-disable-with' attribute
      - Sets disabled property to true
    */
    disableFormElements: function(form) {
      rails.formElements(form, rails.disableSelector).each(function() {
        rails.disableFormElement($(this));
      });
    },

    disableFormElement: function(element) {
      var method, replacement;

      method = element.is('button') ? 'html' : 'val';
      replacement = element.data('disable-with');

      if (replacement !== undefined) {
        element.data('ujs:enable-with', element[method]());
        element[method](replacement);
      }

      element.prop('disabled', true);
      element.data('ujs:disabled', true);
    },

    /* Re-enables disabled form elements:
      - Replaces element text with cached value from 'ujs:enable-with' data store (created in `disableFormElements`)
      - Sets disabled property to false
    */
    enableFormElements: function(form) {
      rails.formElements(form, rails.enableSelector).each(function() {
        rails.enableFormElement($(this));
      });
    },

    enableFormElement: function(element) {
      var method = element.is('button') ? 'html' : 'val';
      if (element.data('ujs:enable-with') !== undefined) {
        element[method](element.data('ujs:enable-with'));
        element.removeData('ujs:enable-with'); // clean up cache
      }
      element.prop('disabled', false);
      element.removeData('ujs:disabled');
    },

   /* For 'data-confirm' attribute:
      - Fires `confirm` event
      - Shows the confirmation dialog
      - Fires the `confirm:complete` event

      Returns `true` if no function stops the chain and user chose yes; `false` otherwise.
      Attaching a handler to the element's `confirm` event that returns a `falsy` value cancels the confirmation dialog.
      Attaching a handler to the element's `confirm:complete` event that returns a `falsy` value makes this function
      return false. The `confirm:complete` event is fired whether or not the user answered true or false to the dialog.
   */
    allowAction: function(element) {
      var message = element.data('confirm'),
          answer = false, callback;
      if (!message) { return true; }

      if (rails.fire(element, 'confirm')) {
        try {
          answer = rails.confirm(message);
        } catch (e) {
          (console.error || console.log).call(console, e.stack || e);
        }
        callback = rails.fire(element, 'confirm:complete', [answer]);
      }
      return answer && callback;
    },

    // Helper function which checks for blank inputs in a form that match the specified CSS selector
    blankInputs: function(form, specifiedSelector, nonBlank) {
      var inputs = $(), input, valueToCheck,
          selector = specifiedSelector || 'input,textarea',
          allInputs = form.find(selector);

      allInputs.each(function() {
        input = $(this);
        valueToCheck = input.is('input[type=checkbox],input[type=radio]') ? input.is(':checked') : !!input.val();
        if (valueToCheck === nonBlank) {

          // Don't count unchecked required radio if other radio with same name is checked
          if (input.is('input[type=radio]') && allInputs.filter('input[type=radio]:checked[name="' + input.attr('name') + '"]').length) {
            return true; // Skip to next input
          }

          inputs = inputs.add(input);
        }
      });
      return inputs.length ? inputs : false;
    },

    // Helper function which checks for non-blank inputs in a form that match the specified CSS selector
    nonBlankInputs: function(form, specifiedSelector) {
      return rails.blankInputs(form, specifiedSelector, true); // true specifies nonBlank
    },

    // Helper function, needed to provide consistent behavior in IE
    stopEverything: function(e) {
      $(e.target).trigger('ujs:everythingStopped');
      e.stopImmediatePropagation();
      return false;
    },

    //  Replace element's html with the 'data-disable-with' after storing original html
    //  and prevent clicking on it
    disableElement: function(element) {
      var replacement = element.data('disable-with');

      if (replacement !== undefined) {
        element.data('ujs:enable-with', element.html()); // store enabled state
        element.html(replacement);
      }

      element.bind('click.railsDisable', function(e) { // prevent further clicking
        return rails.stopEverything(e);
      });
      element.data('ujs:disabled', true);
    },

    // Restore element to its original state which was disabled by 'disableElement' above
    enableElement: function(element) {
      if (element.data('ujs:enable-with') !== undefined) {
        element.html(element.data('ujs:enable-with')); // set to old enabled state
        element.removeData('ujs:enable-with'); // clean up cache
      }
      element.unbind('click.railsDisable'); // enable element
      element.removeData('ujs:disabled');
    }
  };

  if (rails.fire($document, 'rails:attachBindings')) {

    $.ajaxPrefilter(function(options, originalOptions, xhr){ if ( !options.crossDomain ) { rails.CSRFProtection(xhr); }});

    // This event works the same as the load event, except that it fires every
    // time the page is loaded.
    //
    // See https://github.com/rails/jquery-ujs/issues/357
    // See https://developer.mozilla.org/en-US/docs/Using_Firefox_1.5_caching
    $(window).on('pageshow.rails', function () {
      $($.rails.enableSelector).each(function () {
        var element = $(this);

        if (element.data('ujs:disabled')) {
          $.rails.enableFormElement(element);
        }
      });

      $($.rails.linkDisableSelector).each(function () {
        var element = $(this);

        if (element.data('ujs:disabled')) {
          $.rails.enableElement(element);
        }
      });
    });

    $document.delegate(rails.linkDisableSelector, 'ajax:complete', function() {
        rails.enableElement($(this));
    });

    $document.delegate(rails.buttonDisableSelector, 'ajax:complete', function() {
        rails.enableFormElement($(this));
    });

    $document.delegate(rails.linkClickSelector, 'click.rails', function(e) {
      var link = $(this), method = link.data('method'), data = link.data('params'), metaClick = e.metaKey || e.ctrlKey;
      if (!rails.allowAction(link)) return rails.stopEverything(e);

      if (!metaClick && link.is(rails.linkDisableSelector)) rails.disableElement(link);

      if (rails.isRemote(link)) {
        if (metaClick && (!method || method === 'GET') && !data) { return true; }

        var handleRemote = rails.handleRemote(link);
        // Response from rails.handleRemote() will either be false or a deferred object promise.
        if (handleRemote === false) {
          rails.enableElement(link);
        } else {
          handleRemote.fail( function() { rails.enableElement(link); } );
        }
        return false;

      } else if (method) {
        rails.handleMethod(link);
        return false;
      }
    });

    $document.delegate(rails.buttonClickSelector, 'click.rails', function(e) {
      var button = $(this);

      if (!rails.allowAction(button) || !rails.isRemote(button)) return rails.stopEverything(e);

      if (button.is(rails.buttonDisableSelector)) rails.disableFormElement(button);

      var handleRemote = rails.handleRemote(button);
      // Response from rails.handleRemote() will either be false or a deferred object promise.
      if (handleRemote === false) {
        rails.enableFormElement(button);
      } else {
        handleRemote.fail( function() { rails.enableFormElement(button); } );
      }
      return false;
    });

    $document.delegate(rails.inputChangeSelector, 'change.rails', function(e) {
      var link = $(this);
      if (!rails.allowAction(link) || !rails.isRemote(link)) return rails.stopEverything(e);

      rails.handleRemote(link);
      return false;
    });

    $document.delegate(rails.formSubmitSelector, 'submit.rails', function(e) {
      var form = $(this),
        remote = rails.isRemote(form),
        blankRequiredInputs,
        nonBlankFileInputs;

      if (!rails.allowAction(form)) return rails.stopEverything(e);

      // Skip other logic when required values are missing or file upload is present
      if (form.attr('novalidate') === undefined) {
        if (form.data('ujs:formnovalidate-button') === undefined) {
          blankRequiredInputs = rails.blankInputs(form, rails.requiredInputSelector, false);
          if (blankRequiredInputs && rails.fire(form, 'ajax:aborted:required', [blankRequiredInputs])) {
            return rails.stopEverything(e);
          }
        } else {
          // Clear the formnovalidate in case the next button click is not on a formnovalidate button
          // Not strictly necessary to do here, since it is also reset on each button click, but just to be certain
          form.data('ujs:formnovalidate-button', undefined);
        }
      }

      if (remote) {
        nonBlankFileInputs = rails.nonBlankInputs(form, rails.fileInputSelector);
        if (nonBlankFileInputs) {
          // Slight timeout so that the submit button gets properly serialized
          // (make it easy for event handler to serialize form without disabled values)
          setTimeout(function(){ rails.disableFormElements(form); }, 13);
          var aborted = rails.fire(form, 'ajax:aborted:file', [nonBlankFileInputs]);

          // Re-enable form elements if event bindings return false (canceling normal form submission)
          if (!aborted) { setTimeout(function(){ rails.enableFormElements(form); }, 13); }

          return aborted;
        }

        rails.handleRemote(form);
        return false;

      } else {
        // Slight timeout so that the submit button gets properly serialized
        setTimeout(function(){ rails.disableFormElements(form); }, 13);
      }
    });

    $document.delegate(rails.formInputClickSelector, 'click.rails', function(event) {
      var button = $(this);

      if (!rails.allowAction(button)) return rails.stopEverything(event);

      // Register the pressed submit button
      var name = button.attr('name'),
        data = name ? {name:name, value:button.val()} : null;

      var form = button.closest('form');
      if (form.length === 0) {
        form = $('#' + button.attr('form'));
      }
      form.data('ujs:submit-button', data);

      // Save attributes from button
      form.data('ujs:formnovalidate-button', button.attr('formnovalidate'));
      form.data('ujs:submit-button-formaction', button.attr('formaction'));
      form.data('ujs:submit-button-formmethod', button.attr('formmethod'));
    });

    $document.delegate(rails.formSubmitSelector, 'ajax:send.rails', function(event) {
      if (this === event.target) rails.disableFormElements($(this));
    });

    $document.delegate(rails.formSubmitSelector, 'ajax:complete.rails', function(event) {
      if (this === event.target) rails.enableFormElements($(this));
    });

    $(function(){
      rails.refreshCSRFTokens();
    });
  }

})( jQuery );
/* Generated by Opal 0.8.1 */
Opal.modules["native"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_minus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs - rhs : lhs['$-'](rhs);
  }
  function $rb_ge(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs >= rhs : lhs['$>='](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $range = Opal.range, $hash2 = Opal.hash2, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$try_convert', '$native?', '$respond_to?', '$to_n', '$raise', '$inspect', '$Native', '$end_with?', '$define_method', '$[]', '$convert', '$call', '$to_proc', '$new', '$each', '$native_reader', '$native_writer', '$extend', '$alias_method', '$to_a', '$_Array', '$include', '$method_missing', '$bind', '$instance_method', '$[]=', '$slice', '$length', '$enum_for', '$===', '$<<', '$==', '$instance_variable_set', '$members', '$each_with_index', '$each_pair', '$name', '$native_module']);
  (function($base) {
    var self = $module($base, 'Native');

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    Opal.defs(self, '$is_a?', function(object, klass) {
      var self = this;

      
      try {
        return object instanceof self.$try_convert(klass);
      }
      catch (e) {
        return false;
      }
    ;
    });

    Opal.defs(self, '$try_convert', function(value) {
      var self = this;

      
      if (self['$native?'](value)) {
        return value;
      }
      else if (value['$respond_to?']("to_n")) {
        return value.$to_n();
      }
      else {
        return nil;
      }
    ;
    });

    Opal.defs(self, '$convert', function(value) {
      var self = this;

      
      if (self['$native?'](value)) {
        return value;
      }
      else if (value['$respond_to?']("to_n")) {
        return value.$to_n();
      }
      else {
        self.$raise($scope.get('ArgumentError'), "" + (value.$inspect()) + " isn't native");
      }
    ;
    });

    Opal.defs(self, '$call', TMP_1 = function(obj, key, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 2);
      TMP_1.$$p = null;
      
      var prop = obj[key];

      if (prop instanceof Function) {
        var converted = new Array(args.length);

        for (var i = 0, length = args.length; i < length; i++) {
          var item = args[i],
              conv = self.$try_convert(item);

          converted[i] = conv === nil ? item : conv;
        }

        if (block !== nil) {
          converted.push(block);
        }

        return self.$Native(prop.apply(obj, converted));
      }
      else {
        return self.$Native(prop);
      }
    ;
    });

    (function($base) {
      var self = $module($base, 'Helpers');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defn(self, '$alias_native', function(new$, old, options) {
        var $a, $b, TMP_2, $c, TMP_3, $d, TMP_4, self = this, as = nil;

        if (old == null) {
          old = new$
        }
        if (options == null) {
          options = $hash2([], {})
        }
        if ((($a = old['$end_with?']("=")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return ($a = ($b = self).$define_method, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
            if (self["native"] == null) self["native"] = nil;
if (value == null) value = nil;
          self["native"][old['$[]']($range(0, -2, false))] = $scope.get('Native').$convert(value);
            return value;}, TMP_2.$$s = self, TMP_2), $a).call($b, new$)
        } else if ((($a = as = options['$[]']("as")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return ($a = ($c = self).$define_method, $a.$$p = (TMP_3 = function(args){var self = TMP_3.$$s || this, block, $a, $b, $c, value = nil;
            if (self["native"] == null) self["native"] = nil;
args = $slice.call(arguments, 0);
            block = TMP_3.$$p || nil, TMP_3.$$p = null;
          if ((($a = value = ($b = ($c = $scope.get('Native')).$call, $b.$$p = block.$to_proc(), $b).apply($c, [self["native"], old].concat(args))) !== nil && (!$a.$$is_boolean || $a == true))) {
              return as.$new(value.$to_n())
              } else {
              return nil
            }}, TMP_3.$$s = self, TMP_3), $a).call($c, new$)
          } else {
          return ($a = ($d = self).$define_method, $a.$$p = (TMP_4 = function(args){var self = TMP_4.$$s || this, block, $a, $b;
            if (self["native"] == null) self["native"] = nil;
args = $slice.call(arguments, 0);
            block = TMP_4.$$p || nil, TMP_4.$$p = null;
          return ($a = ($b = $scope.get('Native')).$call, $a.$$p = block.$to_proc(), $a).apply($b, [self["native"], old].concat(args))}, TMP_4.$$s = self, TMP_4), $a).call($d, new$)
        };
      });

      Opal.defn(self, '$native_reader', function(names) {
        var $a, $b, TMP_5, self = this;

        names = $slice.call(arguments, 0);
        return ($a = ($b = names).$each, $a.$$p = (TMP_5 = function(name){var self = TMP_5.$$s || this, $a, $b, TMP_6;
if (name == null) name = nil;
        return ($a = ($b = self).$define_method, $a.$$p = (TMP_6 = function(){var self = TMP_6.$$s || this;
            if (self["native"] == null) self["native"] = nil;

          return self.$Native(self["native"][name])}, TMP_6.$$s = self, TMP_6), $a).call($b, name)}, TMP_5.$$s = self, TMP_5), $a).call($b);
      });

      Opal.defn(self, '$native_writer', function(names) {
        var $a, $b, TMP_7, self = this;

        names = $slice.call(arguments, 0);
        return ($a = ($b = names).$each, $a.$$p = (TMP_7 = function(name){var self = TMP_7.$$s || this, $a, $b, TMP_8;
if (name == null) name = nil;
        return ($a = ($b = self).$define_method, $a.$$p = (TMP_8 = function(value){var self = TMP_8.$$s || this;
            if (self["native"] == null) self["native"] = nil;
if (value == null) value = nil;
          return self.$Native(self["native"][name] = value)}, TMP_8.$$s = self, TMP_8), $a).call($b, "" + (name) + "=")}, TMP_7.$$s = self, TMP_7), $a).call($b);
      });

      Opal.defn(self, '$native_accessor', function(names) {
        var $a, $b, self = this;

        names = $slice.call(arguments, 0);
        ($a = self).$native_reader.apply($a, [].concat(names));
        return ($b = self).$native_writer.apply($b, [].concat(names));
      });
    })(self);

    Opal.defs(self, '$included', function(klass) {
      var self = this;

      return klass.$extend($scope.get('Helpers'));
    });

    Opal.defn(self, '$initialize', function(native$) {
      var $a, self = this;

      if ((($a = $scope.get('Kernel')['$native?'](native$)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        $scope.get('Kernel').$raise($scope.get('ArgumentError'), "" + (native$.$inspect()) + " isn't native")
      };
      return self["native"] = native$;
    });

    Opal.defn(self, '$to_n', function() {
      var self = this;
      if (self["native"] == null) self["native"] = nil;

      return self["native"];
    });
  })(self);
  (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_9;

    Opal.defn(self, '$native?', function(value) {
      var self = this;

      return value == null || !value.$$class;
    });

    Opal.defn(self, '$Native', function(obj) {
      var $a, self = this;

      if ((($a = obj == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil
      } else if ((($a = self['$native?'](obj)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return (($scope.get('Native')).$$scope.get('Object')).$new(obj)
        } else {
        return obj
      };
    });

    self.$alias_method("_Array", "Array");

    Opal.defn(self, '$Array', TMP_9 = function(object, args) {
      var $a, $b, self = this, $iter = TMP_9.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_9.$$p = null;
      if ((($a = self['$native?'](object)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = block.$to_proc(), $a).apply($b, [object].concat(args)).$to_a()};
      return self.$_Array(object);
    });
  })(self);
  (function($base, $super) {
    function $Object(){};
    var self = $Object = $klass($base, $super, 'Object', $Object);

    var def = self.$$proto, $scope = self.$$scope, TMP_10, TMP_11, TMP_12;

    def["native"] = nil;
    self.$include(Opal.get('Native'));

    Opal.defn(self, '$==', function(other) {
      var self = this;

      return self["native"] === $scope.get('Native').$try_convert(other);
    });

    Opal.defn(self, '$has_key?', function(name) {
      var self = this;

      return Opal.hasOwnProperty.call(self["native"], name);
    });

    Opal.defn(self, '$key?', def['$has_key?']);

    Opal.defn(self, '$include?', def['$has_key?']);

    Opal.defn(self, '$member?', def['$has_key?']);

    Opal.defn(self, '$each', TMP_10 = function(args) {
      var $a, self = this, $iter = TMP_10.$$p, $yield = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_10.$$p = null;
      if (($yield !== nil)) {
        
        for (var key in self["native"]) {
          ((($a = Opal.yieldX($yield, [key, self["native"][key]])) === $breaker) ? $breaker.$v : $a)
        }
      ;
        return self;
        } else {
        return ($a = self).$method_missing.apply($a, ["each"].concat(args))
      };
    });

    Opal.defn(self, '$[]', function(key) {
      var self = this;

      
      var prop = self["native"][key];

      if (prop instanceof Function) {
        return prop;
      }
      else {
        return Opal.get('Native').$call(self["native"], key)
      }
    ;
    });

    Opal.defn(self, '$[]=', function(key, value) {
      var $a, self = this, native$ = nil;

      native$ = $scope.get('Native').$try_convert(value);
      if ((($a = native$ === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][key] = value;
        } else {
        return self["native"][key] = native$;
      };
    });

    Opal.defn(self, '$merge!', function(other) {
      var self = this;

      
      var other = $scope.get('Native').$convert(other);

      for (var prop in other) {
        self["native"][prop] = other[prop];
      }
    ;
      return self;
    });

    Opal.defn(self, '$respond_to?', function(name, include_all) {
      var self = this;

      if (include_all == null) {
        include_all = false
      }
      return $scope.get('Kernel').$instance_method("respond_to?").$bind(self).$call(name, include_all);
    });

    Opal.defn(self, '$respond_to_missing?', function(name) {
      var self = this;

      return Opal.hasOwnProperty.call(self["native"], name);
    });

    Opal.defn(self, '$method_missing', TMP_11 = function(mid, args) {
      var $a, $b, self = this, $iter = TMP_11.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_11.$$p = null;
      
      if (mid.charAt(mid.length - 1) === '=') {
        return self['$[]='](mid.$slice(0, $rb_minus(mid.$length(), 1)), args['$[]'](0));
      }
      else {
        return ($a = ($b = Opal.get('Native')).$call, $a.$$p = block.$to_proc(), $a).apply($b, [self["native"], mid].concat(args));
      }
    ;
    });

    Opal.defn(self, '$nil?', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$is_a?', function(klass) {
      var self = this;

      return Opal.is_a(self, klass);
    });

    Opal.defn(self, '$kind_of?', def['$is_a?']);

    Opal.defn(self, '$instance_of?', function(klass) {
      var self = this;

      return self.$$class === klass;
    });

    Opal.defn(self, '$class', function() {
      var self = this;

      return self.$$class;
    });

    Opal.defn(self, '$to_a', TMP_12 = function(options) {
      var $a, $b, self = this, $iter = TMP_12.$$p, block = $iter || nil;

      if (options == null) {
        options = $hash2([], {})
      }
      TMP_12.$$p = null;
      return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = block.$to_proc(), $a).call($b, self["native"], options).$to_a();
    });

    return (Opal.defn(self, '$inspect', function() {
      var self = this;

      return "#<Native:" + (String(self["native"])) + ">";
    }), nil) && 'inspect';
  })($scope.get('Native'), $scope.get('BasicObject'));
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope, TMP_13, TMP_14;

    def.named = def["native"] = def.get = def.block = def.set = def.length = nil;
    self.$include($scope.get('Native'));

    self.$include($scope.get('Enumerable'));

    def.$initialize = TMP_13 = function(native$, options) {
      var $a, self = this, $iter = TMP_13.$$p, block = $iter || nil;

      if (options == null) {
        options = $hash2([], {})
      }
      TMP_13.$$p = null;
      Opal.find_super_dispatcher(self, 'initialize', TMP_13, null).apply(self, [native$]);
      self.get = ((($a = options['$[]']("get")) !== false && $a !== nil) ? $a : options['$[]']("access"));
      self.named = options['$[]']("named");
      self.set = ((($a = options['$[]']("set")) !== false && $a !== nil) ? $a : options['$[]']("access"));
      self.length = ((($a = options['$[]']("length")) !== false && $a !== nil) ? $a : "length");
      self.block = block;
      if ((($a = self.$length() == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$raise($scope.get('ArgumentError'), "no length found on the array-like object")
        } else {
        return nil
      };
    };

    def.$each = TMP_14 = function() {
      var self = this, $iter = TMP_14.$$p, block = $iter || nil;

      TMP_14.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each")
      };
      
      for (var i = 0, length = self.$length(); i < length; i++) {
        var value = Opal.yield1(block, self['$[]'](i));

        if (value === $breaker) {
          return $breaker.$v;
        }
      }
    ;
      return self;
    };

    def['$[]'] = function(index) {
      var $a, self = this, result = nil, $case = nil;

      result = (function() {$case = index;if ($scope.get('String')['$===']($case) || $scope.get('Symbol')['$===']($case)) {if ((($a = self.named) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][self.named](index);
        } else {
        return self["native"][index];
      }}else if ($scope.get('Integer')['$===']($case)) {if ((($a = self.get) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][self.get](index);
        } else {
        return self["native"][index];
      }}else { return nil }})();
      if (result !== false && result !== nil) {
        if ((($a = self.block) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self.block.$call(result)
          } else {
          return self.$Native(result)
        }
        } else {
        return nil
      };
    };

    def['$[]='] = function(index, value) {
      var $a, self = this;

      if ((($a = self.set) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][self.set](index, $scope.get('Native').$convert(value));
        } else {
        return self["native"][index] = $scope.get('Native').$convert(value);
      };
    };

    def.$last = function(count) {
      var $a, self = this, index = nil, result = nil;

      if (count == null) {
        count = nil
      }
      if (count !== false && count !== nil) {
        index = $rb_minus(self.$length(), 1);
        result = [];
        while ($rb_ge(index, 0)) {
        result['$<<'](self['$[]'](index));
        index = $rb_minus(index, 1);};
        return result;
        } else {
        return self['$[]']($rb_minus(self.$length(), 1))
      };
    };

    def.$length = function() {
      var self = this;

      return self["native"][self.length];
    };

    Opal.defn(self, '$to_ary', def.$to_a);

    return (def.$inspect = function() {
      var self = this;

      return self.$to_a().$inspect();
    }, nil) && 'inspect';
  })($scope.get('Native'), null);
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Proc(){};
    var self = $Proc = $klass($base, $super, 'Proc', $Proc);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Regexp(){};
    var self = $Regexp = $klass($base, $super, 'Regexp', $Regexp);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $MatchData(){};
    var self = $MatchData = $klass($base, $super, 'MatchData', $MatchData);

    var def = self.$$proto, $scope = self.$$scope;

    def.matches = nil;
    return (def.$to_n = function() {
      var self = this;

      return self.matches;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Struct(){};
    var self = $Struct = $klass($base, $super, 'Struct', $Struct);

    var def = self.$$proto, $scope = self.$$scope;

    def.$initialize = function(args) {
      var $a, $b, TMP_15, $c, TMP_16, self = this, object = nil;

      args = $slice.call(arguments, 0);
      if ((($a = (($b = args.$length()['$=='](1)) ? self['$native?'](args['$[]'](0)) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        object = args['$[]'](0);
        return ($a = ($b = self.$members()).$each, $a.$$p = (TMP_15 = function(name){var self = TMP_15.$$s || this;
if (name == null) name = nil;
        return self.$instance_variable_set("@" + (name), self.$Native(object[name]))}, TMP_15.$$s = self, TMP_15), $a).call($b);
        } else {
        return ($a = ($c = self.$members()).$each_with_index, $a.$$p = (TMP_16 = function(name, index){var self = TMP_16.$$s || this;
if (name == null) name = nil;if (index == null) index = nil;
        return self.$instance_variable_set("@" + (name), args['$[]'](index))}, TMP_16.$$s = self, TMP_16), $a).call($c)
      };
    };

    return (def.$to_n = function() {
      var $a, $b, TMP_17, self = this, result = nil;

      result = {};
      ($a = ($b = self).$each_pair, $a.$$p = (TMP_17 = function(name, value){var self = TMP_17.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
      return result[name] = value.$to_n();}, TMP_17.$$s = self, TMP_17), $a).call($b);
      return result;
    }, nil) && 'to_n';
  })(self, null);
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        var obj = self[i];

        if ((obj)['$respond_to?']("to_n")) {
          result.push((obj).$to_n());
        }
        else {
          result.push(obj);
        }
      }

      return result;
    ;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Boolean(){};
    var self = $Boolean = $klass($base, $super, 'Boolean', $Boolean);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Time(){};
    var self = $Time = $klass($base, $super, 'Time', $Time);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return null;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope, TMP_18;

    def.$initialize = TMP_18 = function(defaults) {
      var self = this, $iter = TMP_18.$$p, block = $iter || nil;

      TMP_18.$$p = null;
      
      if (defaults != null) {
        if (defaults.constructor === Object) {
          var _map = self.map,
              smap = self.smap,
              keys = self.keys,
              map, khash, value;

          for (var key in defaults) {
            value = defaults[key];

            if (key.$$is_string) {
              map = smap;
              khash = key;
            } else {
              map = _map;
              khash = key.$hash();
            }

            if (value && value.constructor === Object) {
              map[khash] = $scope.get('Hash').$new(value);
            }
            else {
              map[khash] = self.$Native(value);
            }

            keys.push(key);
          }
        }
        else {
          self.none = defaults;
        }
      }
      else if (block !== nil) {
        self.proc = block;
      }

      return self;
    
    };

    return (def.$to_n = function() {
      var self = this;

      
      var result = {},
          keys   = self.keys,
          _map   = self.map,
          smap   = self.smap,
          map, khash, value, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key   = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        value = map[khash];

        if ((value)['$respond_to?']("to_n")) {
          result[key] = (value).$to_n();
        }
        else {
          result[key] = value;
        }
      }

      return result;
    ;
    }, nil) && 'to_n';
  })(self, null);
  (function($base, $super) {
    function $Module(){};
    var self = $Module = $klass($base, $super, 'Module', $Module);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$native_module = function() {
      var self = this;

      return Opal.global[self.$name()] = self;
    }, nil) && 'native_module'
  })(self, null);
  (function($base, $super) {
    function $Class(){};
    var self = $Class = $klass($base, $super, 'Class', $Class);

    var def = self.$$proto, $scope = self.$$scope;

    def.$native_alias = function(new_jsid, existing_mid) {
      var self = this;

      
      var aliased = self.$$proto['$' + existing_mid];
      if (!aliased) {
        self.$raise($scope.get('NameError'), "undefined method `" + (existing_mid) + "' for class `" + (self.$inspect()) + "'");
      }
      self.$$proto[new_jsid] = aliased;
    ;
    };

    return (def.$native_class = function() {
      var self = this;

      self.$native_module();
      self["new"] = self.$new;
    }, nil) && 'native_class';
  })(self, null);
  return $gvars.$ = $gvars.global = self.$Native(Opal.global);
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/constants"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var $a, self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require', '$raise']);
  self.$require("native");
  if ((($a = ($scope.JQUERY_CLASS != null)) !== nil && (!$a.$$is_boolean || $a == true))) {
    return nil
    } else {
    return (function() {if ((($a = !!Opal.global.jQuery) !== nil && (!$a.$$is_boolean || $a == true))) {return Opal.cdecl($scope, 'JQUERY_CLASS', Opal.cdecl($scope, 'JQUERY_SELECTOR', Opal.global.jQuery))}else if ((($a = !!Opal.global.Zepto) !== nil && (!$a.$$is_boolean || $a == true))) {Opal.cdecl($scope, 'JQUERY_SELECTOR', Opal.global.Zepto);
    return Opal.cdecl($scope, 'JQUERY_CLASS', Opal.global.Zepto.zepto.Z);}else {return self.$raise($scope.get('NameError'), "Can't find jQuery or Zepto. jQuery must be included before opal-jquery")}})()
  };
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/element"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$to_n', '$include', '$each', '$alias_native', '$attr_reader', '$nil?', '$[]', '$[]=', '$raise', '$is_a?', '$has_key?', '$delete', '$call', '$gsub', '$upcase', '$compact', '$map', '$respond_to?', '$<<', '$Native', '$new']);
  self.$require("native");
  self.$require("opal/jquery/constants");
  return (function($base, $super) {
    function $Element(){};
    var self = $Element = $klass($base, $super, 'Element', $Element);

    var def = self.$$proto, $scope = self.$$scope, TMP_2, TMP_3, TMP_6, TMP_7, TMP_8;

    var $ = $scope.get('JQUERY_SELECTOR').$to_n();

    self.$include($scope.get('Enumerable'));

    Opal.defs(self, '$find', function(selector) {
      var self = this;

      return $(selector);
    });

    Opal.defs(self, '$[]', function(selector) {
      var self = this;

      return $(selector);
    });

    Opal.defs(self, '$id', function(id) {
      var self = this;

      
      var el = document.getElementById(id);

      if (!el) {
        return nil;
      }

      return $(el);
    
    });

    Opal.defs(self, '$new', function(tag) {
      var self = this;

      if (tag == null) {
        tag = "div"
      }
      return $(document.createElement(tag));
    });

    Opal.defs(self, '$parse', function(str) {
      var self = this;

      return $.parseHTML ? $($.parseHTML(str)) : $(str);
    });

    Opal.defs(self, '$expose', function(methods) {
      var $a, $b, TMP_1, self = this;

      methods = $slice.call(arguments, 0);
      return ($a = ($b = methods).$each, $a.$$p = (TMP_1 = function(method){var self = TMP_1.$$s || this;
if (method == null) method = nil;
      return self.$alias_native(method)}, TMP_1.$$s = self, TMP_1), $a).call($b);
    });

    self.$attr_reader("selector");

    self.$alias_native("after");

    self.$alias_native("before");

    self.$alias_native("parent");

    self.$alias_native("parents");

    self.$alias_native("prev");

    self.$alias_native("remove");

    self.$alias_native("hide");

    self.$alias_native("show");

    self.$alias_native("toggle");

    self.$alias_native("children");

    self.$alias_native("blur");

    self.$alias_native("closest");

    self.$alias_native("detach");

    self.$alias_native("focus");

    self.$alias_native("find");

    self.$alias_native("next");

    self.$alias_native("siblings");

    self.$alias_native("text");

    self.$alias_native("trigger");

    self.$alias_native("append");

    self.$alias_native("prepend");

    self.$alias_native("serialize");

    self.$alias_native("is");

    self.$alias_native("filter");

    self.$alias_native("last");

    self.$alias_native("wrap");

    self.$alias_native("stop");

    self.$alias_native("clone");

    self.$alias_native("empty");

    self.$alias_native("get");

    self.$alias_native("prop");

    Opal.defn(self, '$succ', def.$next);

    Opal.defn(self, '$<<', def.$append);

    self.$alias_native("add_class", "addClass");

    self.$alias_native("append_to", "appendTo");

    self.$alias_native("has_class?", "hasClass");

    self.$alias_native("html=", "html");

    self.$alias_native("index");

    self.$alias_native("is?", "is");

    self.$alias_native("remove_attr", "removeAttr");

    self.$alias_native("remove_class", "removeClass");

    self.$alias_native("submit");

    self.$alias_native("text=", "text");

    self.$alias_native("toggle_class", "toggleClass");

    self.$alias_native("value=", "val");

    self.$alias_native("scroll_top=", "scrollTop");

    self.$alias_native("scroll_top", "scrollTop");

    self.$alias_native("scroll_left=", "scrollLeft");

    self.$alias_native("scroll_left", "scrollLeft");

    self.$alias_native("remove_attribute", "removeAttr");

    self.$alias_native("slide_down", "slideDown");

    self.$alias_native("slide_up", "slideUp");

    self.$alias_native("slide_toggle", "slideToggle");

    self.$alias_native("fade_toggle", "fadeToggle");

    self.$alias_native("height=", "height");

    self.$alias_native("width=", "width");

    self.$alias_native("outer_width", "outerWidth");

    self.$alias_native("outer_height", "outerHeight");

    def.$to_n = function() {
      var self = this;

      return self;
    };

    def['$[]'] = function(name) {
      var self = this;

      
      var value = self.attr(name);
      if(value === undefined) return nil;
      return value;
    
    };

    def['$[]='] = function(name, value) {
      var $a, self = this;

      if ((($a = value['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.removeAttr(name);};
      return self.attr(name, value);
    };

    def.$attr = function(args) {
      var self = this;

      args = $slice.call(arguments, 0);
      
      var size = args.length;
      switch (size) {
      case 1:
        return self['$[]'](args[0]);
        break;
      case 2:
        return self['$[]='](args[0], args[1]);
        break;
      default:
        self.$raise($scope.get('ArgumentError'), "#attr only accepts 1 or 2 arguments")
      }
    ;
    };

    def['$has_attribute?'] = function(name) {
      var self = this;

      return self.attr(name) !== undefined;
    };

    def.$append_to_body = function() {
      var self = this;

      return self.appendTo(document.body);
    };

    def.$append_to_head = function() {
      var self = this;

      return self.appendTo(document.head);
    };

    def.$at = function(index) {
      var self = this;

      
      var length = self.length;

      if (index < 0) {
        index += length;
      }

      if (index < 0 || index >= length) {
        return nil;
      }

      return $(self[index]);
    
    };

    def.$class_name = function() {
      var self = this;

      
      var first = self[0];
      return (first && first.className) || "";
    
    };

    def['$class_name='] = function(name) {
      var self = this;

      
      for (var i = 0, length = self.length; i < length; i++) {
        self[i].className = name;
      }
    
      return self;
    };

    def.$css = function(name, value) {
      var $a, $b, self = this;

      if (value == null) {
        value = nil
      }
      if ((($a = ($b = value['$nil?'](), $b !== false && $b !== nil ?name['$is_a?']($scope.get('String')) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.css(name)
      } else if ((($a = name['$is_a?']($scope.get('Hash'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.css(name.$to_n());
        } else {
        self.css(name, value);
      };
      return self;
    };

    def.$animate = TMP_2 = function(params) {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil, speed = nil;

      TMP_2.$$p = null;
      speed = (function() {if ((($a = params['$has_key?']("speed")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return params.$delete("speed")
        } else {
        return 400
      }; return nil; })();
      
      self.animate(params.$to_n(), speed, function() {
        (function() {if ((block !== nil)) {
        return block.$call()
        } else {
        return nil
      }; return nil; })()
      })
    ;
    };

    def.$data = function(args) {
      var self = this;

      args = $slice.call(arguments, 0);
      
      var result = self.data.apply(self, args);
      return result == null ? nil : result;
    
    };

    def.$effect = TMP_3 = function(name, args) {
      var $a, $b, TMP_4, $c, TMP_5, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_3.$$p = null;
      name = ($a = ($b = name).$gsub, $a.$$p = (TMP_4 = function(match){var self = TMP_4.$$s || this;
if (match == null) match = nil;
      return match['$[]'](1).$upcase()}, TMP_4.$$s = self, TMP_4), $a).call($b, /_\w/);
      args = ($a = ($c = args).$map, $a.$$p = (TMP_5 = function(a){var self = TMP_5.$$s || this, $a;
if (a == null) a = nil;
      if ((($a = a['$respond_to?']("to_n")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return a.$to_n()
          } else {
          return nil
        }}, TMP_5.$$s = self, TMP_5), $a).call($c).$compact();
      args['$<<'](function() { (function() {if ((block !== nil)) {
        return block.$call()
        } else {
        return nil
      }; return nil; })() });
      return self[name].apply(self, args);
    };

    def['$visible?'] = function() {
      var self = this;

      return self.is(':visible');
    };

    def.$offset = function() {
      var self = this;

      return self.$Native(self.offset());
    };

    def.$each = TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, $yield = $iter || nil;

      TMP_6.$$p = null;
      for (var i = 0, length = self.length; i < length; i++) {
      if (Opal.yield1($yield, $(self[i])) === $breaker) return $breaker.$v;
      };
      return self;
    };

    def.$first = function() {
      var self = this;

      return self.length ? self.first() : nil;
    };

    def.$html = function(content) {
      var self = this;

      
      if (content != null) {
        return self.html(content);
      }

      return self.html() || '';
    
    };

    def.$id = function() {
      var self = this;

      
      var first = self[0];
      return (first && first.id) || "";
    
    };

    def['$id='] = function(id) {
      var self = this;

      
      var first = self[0];

      if (first) {
        first.id = id;
      }

      return self;
    
    };

    def.$tag_name = function() {
      var self = this;

      return self.length > 0 ? self[0].tagName.toLowerCase() : nil;
    };

    def.$inspect = function() {
      var self = this;

      
      if      (self[0] === document) return '#<Element [document]>'
      else if (self[0] === window  ) return '#<Element [window]>'

      var val, el, str, result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        el  = self[i];
        if (!el.tagName) { return '#<Element ['+el.toString()+']'; }

        str = "<" + el.tagName.toLowerCase();

        if (val = el.id) str += (' id="' + val + '"');
        if (val = el.className) str += (' class="' + val + '"');

        result.push(str + '>');
      }

      return '#<Element [' + result.join(', ') + ']>';
    
    };

    def.$to_s = function() {
      var self = this;

      
      var val, el, result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        el  = self[i];

        result.push(el.outerHTML)
      }

      return result.join(', ');
    
    };

    def.$length = function() {
      var self = this;

      return self.length;
    };

    def['$any?'] = function() {
      var self = this;

      return self.length > 0;
    };

    def['$empty?'] = function() {
      var self = this;

      return self.length === 0;
    };

    Opal.defn(self, '$empty?', def['$none?']);

    def.$on = TMP_7 = function(name, sel) {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      if (sel == null) {
        sel = nil
      }
      TMP_7.$$p = null;
      
      var wrapper = function(evt) {
        if (evt.preventDefault) {
          evt = $scope.get('Event').$new(evt);
        }

        return block.apply(null, arguments);
      };

      block._jq_wrap = wrapper;

      if (sel == nil) {
        self.on(name, wrapper);
      }
      else {
        self.on(name, sel, wrapper);
      }
    ;
      return block;
    };

    def.$one = TMP_8 = function(name, sel) {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      if (sel == null) {
        sel = nil
      }
      TMP_8.$$p = null;
      
      var wrapper = function(evt) {
        if (evt.preventDefault) {
          evt = $scope.get('Event').$new(evt);
        }

        return block.apply(null, arguments);
      };

      block._jq_wrap = wrapper;

      if (sel == nil) {
        self.one(name, wrapper);
      }
      else {
        self.one(name, sel, wrapper);
      }
    ;
      return block;
    };

    def.$off = function(name, sel, block) {
      var self = this;

      if (block == null) {
        block = nil
      }
      
      if (sel == null) {
        return self.off(name);
      }
      else if (block === nil) {
        return self.off(name, sel._jq_wrap);
      }
      else {
        return self.off(name, sel, block._jq_wrap);
      }
    
    };

    def.$serialize_array = function() {
      var $a, $b, TMP_9, self = this;

      return ($a = ($b = (self.serializeArray())).$map, $a.$$p = (TMP_9 = function(e){var self = TMP_9.$$s || this;
if (e == null) e = nil;
      return $scope.get('Hash').$new(e)}, TMP_9.$$s = self, TMP_9), $a).call($b);
    };

    Opal.defn(self, '$size', def.$length);

    def.$value = function() {
      var self = this;

      return self.val() || "";
    };

    def.$height = function() {
      var self = this;

      return self.height() || nil;
    };

    def.$width = function() {
      var self = this;

      return self.width() || nil;
    };

    return (def.$position = function() {
      var self = this;

      return self.$Native(self.position());
    }, nil) && 'position';
  })(self, $scope.get('JQUERY_CLASS').$to_n());
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/window"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$include', '$find', '$on', '$to_proc', '$element', '$off', '$trigger', '$new']);
  self.$require("opal/jquery/element");
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

      def.element = nil;
      self.$include($scope.get('Native'));

      def.$element = function() {
        var $a, self = this;

        return ((($a = self.element) !== false && $a !== nil) ? $a : self.element = $scope.get('Element').$find(window));
      };

      def.$on = TMP_1 = function(args) {
        var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

        args = $slice.call(arguments, 0);
        TMP_1.$$p = null;
        return ($a = ($b = self.$element()).$on, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      };

      def.$off = TMP_2 = function(args) {
        var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

        args = $slice.call(arguments, 0);
        TMP_2.$$p = null;
        return ($a = ($b = self.$element()).$off, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      };

      return (def.$trigger = function(args) {
        var $a, self = this;

        args = $slice.call(arguments, 0);
        return ($a = self.$element()).$trigger.apply($a, [].concat(args));
      }, nil) && 'trigger';
    })(self, null)
  })(self);
  Opal.cdecl($scope, 'Window', (($scope.get('Browser')).$$scope.get('Window')).$new(window));
  return $gvars.window = $scope.get('Window');
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/document"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$to_n', '$call', '$new', '$ready?', '$resolve', '$module_function', '$find', '$extend']);
  self.$require("opal/jquery/constants");
  self.$require("opal/jquery/element");
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DocumentMethods');

      var def = self.$$proto, $scope = self.$$scope, TMP_1, $a, $b, TMP_3;

      var $ = $scope.get('JQUERY_SELECTOR').$to_n();

      Opal.defn(self, '$ready?', TMP_1 = function() {
        var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

        TMP_1.$$p = null;
        if ((block !== nil)) {
          if ((($a = (($b = Opal.cvars['@@__isReady']) == null ? nil : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return block.$call()
            } else {
            return $(block);
          }
          } else {
          return nil
        };
      });

      Opal.defn(self, '$ready', function() {
        var $a, $b, TMP_2, self = this, promise = nil;

        promise = $scope.get('Promise').$new();
        ($a = ($b = $scope.get('Document'))['$ready?'], $a.$$p = (TMP_2 = function(){var self = TMP_2.$$s || this;

        return promise.$resolve()}, TMP_2.$$s = self, TMP_2), $a).call($b);
        return promise;
      });

      self.$module_function("ready?");

      ($a = ($b = self)['$ready?'], $a.$$p = (TMP_3 = function(){var self = TMP_3.$$s || this;

      return (Opal.cvars['@@__isReady'] = true)}, TMP_3.$$s = self, TMP_3), $a).call($b);

      Opal.defn(self, '$title', function() {
        var self = this;

        return document.title;
      });

      Opal.defn(self, '$title=', function(title) {
        var self = this;

        return document.title = title;
      });

      Opal.defn(self, '$head', function() {
        var self = this;

        return $scope.get('Element').$find(document.head);
      });

      Opal.defn(self, '$body', function() {
        var self = this;

        return $scope.get('Element').$find(document.body);
      });
    })(self)
  })(self);
  Opal.cdecl($scope, 'Document', $scope.get('Element').$find(document));
  $scope.get('Document').$extend((($scope.get('Browser')).$$scope.get('DocumentMethods')));
  return $gvars.document = $scope.get('Document');
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/event"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$to_n', '$stop', '$prevent']);
  self.$require("opal/jquery/constants");
  return (function($base, $super) {
    function $Event(){};
    var self = $Event = $klass($base, $super, 'Event', $Event);

    var def = self.$$proto, $scope = self.$$scope;

    def["native"] = nil;
    var $ = $scope.get('JQUERY_SELECTOR').$to_n();

    def.$initialize = function(native$) {
      var self = this;

      return self["native"] = native$;
    };

    def.$to_n = function() {
      var self = this;

      return self["native"];
    };

    def['$[]'] = function(name) {
      var self = this;

      return self["native"][name];
    };

    def.$type = function() {
      var self = this;

      return self["native"].type;
    };

    def.$element = function() {
      var self = this;

      return $(self["native"].currentTarget);
    };

    Opal.defn(self, '$current_target', def.$element);

    def.$target = function() {
      var self = this;

      return $(self["native"].target);
    };

    def['$prevented?'] = function() {
      var self = this;

      return self["native"].isDefaultPrevented();
    };

    def.$prevent = function() {
      var self = this;

      return self["native"].preventDefault();
    };

    def['$stopped?'] = function() {
      var self = this;

      return self["native"].isPropagationStopped();
    };

    def.$stop = function() {
      var self = this;

      return self["native"].stopPropagation();
    };

    def.$stop_immediate = function() {
      var self = this;

      return self["native"].stopImmediatePropagation();
    };

    def.$kill = function() {
      var self = this;

      self.$stop();
      return self.$prevent();
    };

    def.$page_x = function() {
      var self = this;

      return self["native"].pageX;
    };

    def.$page_y = function() {
      var self = this;

      return self["native"].pageY;
    };

    def.$touch_x = function() {
      var self = this;

      return self["native"].originalEvent.touches[0].pageX;
    };

    def.$touch_y = function() {
      var self = this;

      return self["native"].originalEvent.touches[0].pageY;
    };

    def.$ctrl_key = function() {
      var self = this;

      return self["native"].ctrlKey;
    };

    def.$meta_key = function() {
      var self = this;

      return self["native"].metaKey;
    };

    def.$alt_key = function() {
      var self = this;

      return self["native"].altKey;
    };

    def.$shift_key = function() {
      var self = this;

      return self["native"].shiftKey;
    };

    def.$key_code = function() {
      var self = this;

      return self["native"].keyCode;
    };

    def.$which = function() {
      var self = this;

      return self["native"].which;
    };

    Opal.defn(self, '$default_prevented?', def['$prevented?']);

    Opal.defn(self, '$prevent_default', def.$prevent);

    Opal.defn(self, '$propagation_stopped?', def['$stopped?']);

    Opal.defn(self, '$stop_propagation', def.$stop);

    return Opal.defn(self, '$stop_immediate_propagation', def.$stop_immediate);
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["json"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $hash2 = Opal.hash2, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$push', '$[]=', '$[]', '$create_id', '$json_create', '$attr_accessor', '$create_id=', '$===', '$parse', '$generate', '$from_object', '$to_json', '$responds_to?', '$to_io', '$write', '$to_s', '$to_a', '$strftime']);
  (function($base) {
    var self = $module($base, 'JSON');

    var def = self.$$proto, $scope = self.$$scope, $a, $b;

    
    var $parse  = JSON.parse,
        $hasOwn = Opal.hasOwnProperty;

    function to_opal(value, options) {
      switch (typeof value) {
        case 'string':
          return value;

        case 'number':
          return value;

        case 'boolean':
          return !!value;

        case 'null':
          return nil;

        case 'object':
          if (!value) return nil;

          if (value.$$is_array) {
            var arr = (options.array_class).$new();

            for (var i = 0, ii = value.length; i < ii; i++) {
              (arr).$push(to_opal(value[i], options));
            }

            return arr;
          }
          else {
            var hash = (options.object_class).$new();

            for (var k in value) {
              if ($hasOwn.call(value, k)) {
                (hash)['$[]='](k, to_opal(value[k], options));
              }
            }

            var klass;
            if ((klass = (hash)['$[]']($scope.get('JSON').$create_id())) != nil) {
              klass = Opal.cget(klass);
              return (klass).$json_create(hash);
            }
            else {
              return hash;
            }
          }
      }
    };
  

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      return self.$attr_accessor("create_id")
    })(self.$singleton_class());

    (($a = ["json_class"]), $b = self, $b['$create_id='].apply($b, $a), $a[$a.length-1]);

    Opal.defs(self, '$[]', function(value, options) {
      var $a, self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      if ((($a = $scope.get('String')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$parse(value, options)
        } else {
        return self.$generate(value, options)
      };
    });

    Opal.defs(self, '$parse', function(source, options) {
      var self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      return self.$from_object($parse(source), options);
    });

    Opal.defs(self, '$parse!', function(source, options) {
      var self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      return self.$parse(source, options);
    });

    Opal.defs(self, '$from_object', function(js_object, options) {
      var $a, $b, $c, self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      ($a = "object_class", $b = options, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, $scope.get('Hash'))));
      ($a = "array_class", $b = options, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, $scope.get('Array'))));
      return to_opal(js_object, options.smap);
    });

    Opal.defs(self, '$generate', function(obj, options) {
      var self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      return obj.$to_json(options);
    });

    Opal.defs(self, '$dump', function(obj, io, limit) {
      var $a, self = this, string = nil;

      if (io == null) {
        io = nil
      }
      if (limit == null) {
        limit = nil
      }
      string = self.$generate(obj);
      if (io !== false && io !== nil) {
        if ((($a = io['$responds_to?']("to_io")) !== nil && (!$a.$$is_boolean || $a == true))) {
          io = io.$to_io()};
        io.$write(string);
        return io;
        } else {
        return string
      };
    });
  })(self);
  (function($base, $super) {
    function $Object(){};
    var self = $Object = $klass($base, $super, 'Object', $Object);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defn(self, '$to_json', function() {
      var self = this;

      return self.$to_s().$to_json();
    }), nil) && 'to_json'
  })(self, null);
  (function($base) {
    var self = $module($base, 'Enumerable');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defn(self, '$to_json', function() {
      var self = this;

      return self.$to_a().$to_json();
    })
  })(self);
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        result.push((self[i]).$to_json());
      }

      return '[' + result.join(', ') + ']';
    
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $Boolean(){};
    var self = $Boolean = $klass($base, $super, 'Boolean', $Boolean);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return (self == true) ? 'true' : 'false';
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      
      var inspect = [],
          keys = self.keys,
          _map = self.map,
          smap = self.smap,
          map, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        var key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        inspect.push((key).$to_s().$to_json() + ':' + (map[khash]).$to_json());
      }

      return '{' + inspect.join(', ') + '}';
    ;
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return "null";
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return self.toString();
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return Opal.defn(self, '$to_json', def.$inspect)
  })(self, null);
  (function($base, $super) {
    function $Time(){};
    var self = $Time = $klass($base, $super, 'Time', $Time);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return self.$strftime("%FT%T%z").$to_json();
    }, nil) && 'to_json'
  })(self, null);
  return (function($base, $super) {
    function $Date(){};
    var self = $Date = $klass($base, $super, 'Date', $Date);

    var def = self.$$proto, $scope = self.$$scope;

    def.$to_json = function() {
      var self = this;

      return self.$to_s().$to_json();
    };

    return (def.$as_json = function() {
      var self = this;

      return self.$to_s();
    }, nil) && 'as_json';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["promise"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  function $rb_plus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs + rhs : lhs['$+'](rhs);
  }
  function $rb_le(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs <= rhs : lhs['$<='](rhs);
  }
  function $rb_minus(lhs, rhs) {
    return (typeof(lhs) === 'number' && typeof(rhs) === 'number') ? lhs - rhs : lhs['$-'](rhs);
  }
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$resolve', '$new', '$reject', '$attr_reader', '$===', '$value', '$has_key?', '$keys', '$!', '$==', '$<<', '$>>', '$exception?', '$[]', '$resolved?', '$rejected?', '$error', '$include?', '$action', '$realized?', '$raise', '$^', '$call', '$resolve!', '$exception!', '$reject!', '$class', '$object_id', '$inspect', '$act?', '$nil?', '$prev', '$push', '$concat', '$it', '$lambda', '$reverse', '$pop', '$length', '$shift', '$each', '$wait', '$then', '$to_proc', '$map', '$reduce', '$always', '$try', '$tap', '$all?', '$find']);
  return (function($base, $super) {
    function $Promise(){};
    var self = $Promise = $klass($base, $super, 'Promise', $Promise);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    def.value = def.action = def.exception = def.realized = def.delayed = def.error = def.prev = def.next = nil;
    Opal.defs(self, '$value', function(value) {
      var self = this;

      return self.$new().$resolve(value);
    });

    Opal.defs(self, '$error', function(value) {
      var self = this;

      return self.$new().$reject(value);
    });

    Opal.defs(self, '$when', function(promises) {
      var self = this;

      promises = $slice.call(arguments, 0);
      return $scope.get('When').$new(promises);
    });

    self.$attr_reader("error", "prev", "next");

    def.$initialize = function(action) {
      var self = this;

      if (action == null) {
        action = $hash2([], {})
      }
      self.action = action;
      self.realized = false;
      self.exception = false;
      self.value = nil;
      self.error = nil;
      self.delayed = false;
      self.prev = nil;
      return self.next = nil;
    };

    def.$value = function() {
      var $a, self = this;

      if ((($a = $scope.get('Promise')['$==='](self.value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.value.$value()
        } else {
        return self.value
      };
    };

    def['$act?'] = function() {
      var $a, self = this;

      return ((($a = self.action['$has_key?']("success")) !== false && $a !== nil) ? $a : self.action['$has_key?']("always"));
    };

    def.$action = function() {
      var self = this;

      return self.action.$keys();
    };

    def['$exception?'] = function() {
      var self = this;

      return self.exception;
    };

    def['$realized?'] = function() {
      var self = this;

      return self.realized['$!']()['$!']();
    };

    def['$resolved?'] = function() {
      var self = this;

      return self.realized['$==']("resolve");
    };

    def['$rejected?'] = function() {
      var self = this;

      return self.realized['$==']("reject");
    };

    def['$^'] = function(promise) {
      var self = this;

      promise['$<<'](self);
      self['$>>'](promise);
      return promise;
    };

    def['$<<'] = function(promise) {
      var self = this;

      self.prev = promise;
      return self;
    };

    def['$>>'] = function(promise) {
      var $a, $b, $c, self = this;

      self.next = promise;
      if ((($a = self['$exception?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        promise.$reject(self.delayed['$[]'](0))
      } else if ((($a = self['$resolved?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        promise.$resolve((function() {if ((($a = self.delayed) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self.delayed['$[]'](0)
          } else {
          return self.$value()
        }; return nil; })())
      } else if ((($a = self['$rejected?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = ((($b = self.action['$has_key?']("failure")['$!']()) !== false && $b !== nil) ? $b : $scope.get('Promise')['$==='](((function() {if ((($c = self.delayed) !== nil && (!$c.$$is_boolean || $c == true))) {
          return self.delayed['$[]'](0)
          } else {
          return self.error
        }; return nil; })())))) !== nil && (!$a.$$is_boolean || $a == true))) {
          promise.$reject((function() {if ((($a = self.delayed) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.delayed['$[]'](0)
            } else {
            return self.$error()
          }; return nil; })())
        } else if ((($a = promise.$action()['$include?']("always")) !== nil && (!$a.$$is_boolean || $a == true))) {
          promise.$reject((function() {if ((($a = self.delayed) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.delayed['$[]'](0)
            } else {
            return self.$error()
          }; return nil; })())}};
      return self;
    };

    def.$resolve = function(value) {
      var $a, $b, self = this, block = nil, e = nil;

      if (value == null) {
        value = nil
      }
      if ((($a = self['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "the promise has already been realized")};
      if ((($a = $scope.get('Promise')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return (value['$<<'](self.prev))['$^'](self)};
      try {
      if ((($a = block = ((($b = self.action['$[]']("success")) !== false && $b !== nil) ? $b : self.action['$[]']("always"))) !== nil && (!$a.$$is_boolean || $a == true))) {
          value = block.$call(value)};
        self['$resolve!'](value);
      } catch ($err) {if (Opal.rescue($err, [$scope.get('Exception')])) {e = $err;
        self['$exception!'](e)
        }else { throw $err; }
      };
      return self;
    };

    def['$resolve!'] = function(value) {
      var $a, self = this;

      self.realized = "resolve";
      self.value = value;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.next.$resolve(value)
        } else {
        return self.delayed = [value]
      };
    };

    def.$reject = function(value) {
      var $a, $b, self = this, block = nil, e = nil;

      if (value == null) {
        value = nil
      }
      if ((($a = self['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "the promise has already been realized")};
      if ((($a = $scope.get('Promise')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return (value['$<<'](self.prev))['$^'](self)};
      try {
      if ((($a = block = ((($b = self.action['$[]']("failure")) !== false && $b !== nil) ? $b : self.action['$[]']("always"))) !== nil && (!$a.$$is_boolean || $a == true))) {
          value = block.$call(value)};
        if ((($a = self.action['$has_key?']("always")) !== nil && (!$a.$$is_boolean || $a == true))) {
          self['$resolve!'](value)
          } else {
          self['$reject!'](value)
        };
      } catch ($err) {if (Opal.rescue($err, [$scope.get('Exception')])) {e = $err;
        self['$exception!'](e)
        }else { throw $err; }
      };
      return self;
    };

    def['$reject!'] = function(value) {
      var $a, self = this;

      self.realized = "reject";
      self.error = value;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.next.$reject(value)
        } else {
        return self.delayed = [value]
      };
    };

    def['$exception!'] = function(error) {
      var self = this;

      self.exception = true;
      return self['$reject!'](error);
    };

    def.$then = TMP_1 = function() {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Promise').$new($hash2(["success"], {"success": block})));
    };

    Opal.defn(self, '$do', def.$then);

    def.$fail = TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Promise').$new($hash2(["failure"], {"failure": block})));
    };

    Opal.defn(self, '$rescue', def.$fail);

    Opal.defn(self, '$catch', def.$fail);

    def.$always = TMP_3 = function() {
      var $a, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Promise').$new($hash2(["always"], {"always": block})));
    };

    Opal.defn(self, '$finally', def.$always);

    Opal.defn(self, '$ensure', def.$always);

    def.$trace = TMP_4 = function(depth) {
      var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      if (depth == null) {
        depth = nil
      }
      TMP_4.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Trace').$new(depth, block));
    };

    def.$inspect = function() {
      var $a, self = this, result = nil;

      result = "#<" + (self.$class()) + "(" + (self.$object_id()) + ")";
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = $rb_plus(result, " >> " + (self.next.$inspect()))};
      if ((($a = self['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = $rb_plus(result, ": " + ((((($a = self.value) !== false && $a !== nil) ? $a : self.error)).$inspect()) + ">")
        } else {
        result = $rb_plus(result, ">")
      };
      return result;
    };

    (function($base, $super) {
      function $Trace(){};
      var self = $Trace = $klass($base, $super, 'Trace', $Trace);

      var def = self.$$proto, $scope = self.$$scope, TMP_6;

      Opal.defs(self, '$it', function(promise) {
        var $a, $b, self = this, current = nil, prev = nil;

        current = [];
        if ((($a = ((($b = promise['$act?']()) !== false && $b !== nil) ? $b : promise.$prev()['$nil?']())) !== nil && (!$a.$$is_boolean || $a == true))) {
          current.$push(promise.$value())};
        if ((($a = prev = promise.$prev()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return current.$concat(self.$it(prev))
          } else {
          return current
        };
      });

      return (def.$initialize = TMP_6 = function(depth, block) {
        var $a, $b, TMP_5, self = this, $iter = TMP_6.$$p, $yield = $iter || nil;

        TMP_6.$$p = null;
        self.depth = depth;
        return Opal.find_super_dispatcher(self, 'initialize', TMP_6, null).apply(self, [$hash2(["success"], {"success": ($a = ($b = self).$lambda, $a.$$p = (TMP_5 = function(){var self = TMP_5.$$s || this, $a, $b, trace = nil;

        trace = $scope.get('Trace').$it(self).$reverse();
          trace.$pop();
          if ((($a = (($b = depth !== false && depth !== nil) ? $rb_le(depth, trace.$length()) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            trace.$shift($rb_minus(trace.$length(), depth))};
          return ($a = block).$call.apply($a, [].concat(trace));}, TMP_5.$$s = self, TMP_5), $a).call($b)})]);
      }, nil) && 'initialize';
    })(self, self);

    return (function($base, $super) {
      function $When(){};
      var self = $When = $klass($base, $super, 'When', $When);

      var def = self.$$proto, $scope = self.$$scope, TMP_7, TMP_9, TMP_11, TMP_13, TMP_17;

      def.wait = nil;
      def.$initialize = TMP_7 = function(promises) {
        var $a, $b, TMP_8, self = this, $iter = TMP_7.$$p, $yield = $iter || nil;

        if (promises == null) {
          promises = []
        }
        TMP_7.$$p = null;
        Opal.find_super_dispatcher(self, 'initialize', TMP_7, null).apply(self, []);
        self.wait = [];
        return ($a = ($b = promises).$each, $a.$$p = (TMP_8 = function(promise){var self = TMP_8.$$s || this;
if (promise == null) promise = nil;
        return self.$wait(promise)}, TMP_8.$$s = self, TMP_8), $a).call($b);
      };

      def.$each = TMP_9 = function() {
        var $a, $b, TMP_10, self = this, $iter = TMP_9.$$p, block = $iter || nil;

        TMP_9.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "no block given")
        };
        return ($a = ($b = self).$then, $a.$$p = (TMP_10 = function(values){var self = TMP_10.$$s || this, $a, $b;
if (values == null) values = nil;
        return ($a = ($b = values).$each, $a.$$p = block.$to_proc(), $a).call($b)}, TMP_10.$$s = self, TMP_10), $a).call($b);
      };

      def.$collect = TMP_11 = function() {
        var $a, $b, TMP_12, self = this, $iter = TMP_11.$$p, block = $iter || nil;

        TMP_11.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "no block given")
        };
        return ($a = ($b = self).$then, $a.$$p = (TMP_12 = function(values){var self = TMP_12.$$s || this, $a, $b;
if (values == null) values = nil;
        return $scope.get('When').$new(($a = ($b = values).$map, $a.$$p = block.$to_proc(), $a).call($b))}, TMP_12.$$s = self, TMP_12), $a).call($b);
      };

      def.$inject = TMP_13 = function(args) {
        var $a, $b, TMP_14, self = this, $iter = TMP_13.$$p, block = $iter || nil;

        args = $slice.call(arguments, 0);
        TMP_13.$$p = null;
        return ($a = ($b = self).$then, $a.$$p = (TMP_14 = function(values){var self = TMP_14.$$s || this, $a, $b;
if (values == null) values = nil;
        return ($a = ($b = values).$reduce, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args))}, TMP_14.$$s = self, TMP_14), $a).call($b);
      };

      Opal.defn(self, '$map', def.$collect);

      Opal.defn(self, '$reduce', def.$inject);

      def.$wait = function(promise) {
        var $a, $b, TMP_15, self = this;

        if ((($a = $scope.get('Promise')['$==='](promise)) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          promise = $scope.get('Promise').$value(promise)
        };
        if ((($a = promise['$act?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          promise = promise.$then()};
        self.wait['$<<'](promise);
        ($a = ($b = promise).$always, $a.$$p = (TMP_15 = function(){var self = TMP_15.$$s || this, $a;
          if (self.next == null) self.next = nil;

        if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$try()
            } else {
            return nil
          }}, TMP_15.$$s = self, TMP_15), $a).call($b);
        return self;
      };

      Opal.defn(self, '$and', def.$wait);

      def['$>>'] = TMP_17 = function() {var $zuper = $slice.call(arguments, 0);
        var $a, $b, TMP_16, self = this, $iter = TMP_17.$$p, $yield = $iter || nil;

        TMP_17.$$p = null;
        return ($a = ($b = Opal.find_super_dispatcher(self, '>>', TMP_17, $iter).apply(self, $zuper)).$tap, $a.$$p = (TMP_16 = function(){var self = TMP_16.$$s || this;

        return self.$try()}, TMP_16.$$s = self, TMP_16), $a).call($b);
      };

      return (def.$try = function() {
        var $a, $b, $c, $d, self = this, promise = nil;

        if ((($a = ($b = ($c = self.wait)['$all?'], $b.$$p = "realized?".$to_proc(), $b).call($c)) !== nil && (!$a.$$is_boolean || $a == true))) {
          if ((($a = promise = ($b = ($d = self.wait).$find, $b.$$p = "rejected?".$to_proc(), $b).call($d)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$reject(promise.$error())
            } else {
            return self.$resolve(($a = ($b = self.wait).$map, $a.$$p = "value".$to_proc(), $a).call($b))
          }
          } else {
          return nil
        };
      }, nil) && 'try';
    })(self, self);
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/http"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$to_n', '$each', '$define_singleton_method', '$send', '$new', '$define_method', '$attr_reader', '$delete', '$update', '$upcase', '$succeed', '$fail', '$promise', '$parse', '$private', '$tap', '$proc', '$ok?', '$resolve', '$reject', '$from_object', '$call']);
  self.$require("json");
  self.$require("native");
  self.$require("promise");
  self.$require("opal/jquery/constants");
  return (function($base, $super) {
    function $HTTP(){};
    var self = $HTTP = $klass($base, $super, 'HTTP', $HTTP);

    var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

    def.settings = def.payload = def.url = def.method = def.handler = def.json = def.body = def.ok = def.xhr = def.promise = def.status_code = nil;
    var $ = $scope.get('JQUERY_SELECTOR').$to_n();

    Opal.cdecl($scope, 'ACTIONS', ["get", "post", "put", "delete", "patch", "head"]);

    ($a = ($b = $scope.get('ACTIONS')).$each, $a.$$p = (TMP_1 = function(action){var self = TMP_1.$$s || this, $a, $b, TMP_2, $c, TMP_3;
if (action == null) action = nil;
    ($a = ($b = self).$define_singleton_method, $a.$$p = (TMP_2 = function(url, options){var self = TMP_2.$$s || this, block;
if (url == null) url = nil;if (options == null) options = $hash2([], {});
        block = TMP_2.$$p || nil, TMP_2.$$p = null;
      return self.$new().$send(action, url, options, block)}, TMP_2.$$s = self, TMP_2), $a).call($b, action);
      return ($a = ($c = self).$define_method, $a.$$p = (TMP_3 = function(url, options){var self = TMP_3.$$s || this, block;
if (url == null) url = nil;if (options == null) options = $hash2([], {});
        block = TMP_3.$$p || nil, TMP_3.$$p = null;
      return self.$send(action, url, options, block)}, TMP_3.$$s = self, TMP_3), $a).call($c, action);}, TMP_1.$$s = self, TMP_1), $a).call($b);

    Opal.defs(self, '$setup', function() {
      var self = this;

      return $scope.get('Hash').$new($.ajaxSetup());
    });

    Opal.defs(self, '$setup=', function(settings) {
      var self = this;

      return $.ajaxSetup(settings.$to_n());
    });

    self.$attr_reader("body", "error_message", "method", "status_code", "url", "xhr");

    def.$initialize = function() {
      var self = this;

      self.settings = $hash2([], {});
      return self.ok = true;
    };

    def.$send = function(method, url, options, block) {
      var $a, self = this, settings = nil, payload = nil;

      self.method = method;
      self.url = url;
      self.payload = options.$delete("payload");
      self.handler = block;
      self.settings.$update(options);
      $a = [self.settings.$to_n(), self.payload], settings = $a[0], payload = $a[1];
      
      if (typeof(payload) === 'string') {
        settings.data = payload;
      }
      else if (payload != nil) {
        settings.data = payload.$to_json();
        settings.contentType = 'application/json';
      }

      settings.url  = self.url;
      settings.type = self.method.$upcase();

      settings.success = function(data, status, xhr) {
        return self.$succeed(data, status, xhr);
      };

      settings.error = function(xhr, status, error) {
        return self.$fail(xhr, status, error);
      };

      $.ajax(settings);
    ;
      if ((($a = self.handler) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self
        } else {
        return self.$promise()
      };
    };

    def.$json = function() {
      var $a, self = this;

      return ((($a = self.json) !== false && $a !== nil) ? $a : self.json = $scope.get('JSON').$parse(self.body));
    };

    def['$ok?'] = function() {
      var self = this;

      return self.ok;
    };

    def.$get_header = function(key) {
      var self = this;

      return self.xhr.getResponseHeader(key);;
    };

    self.$private();

    def.$promise = function() {
      var $a, $b, TMP_4, self = this;

      if ((($a = self.promise) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.promise};
      return self.promise = ($a = ($b = $scope.get('Promise').$new()).$tap, $a.$$p = (TMP_4 = function(promise){var self = TMP_4.$$s || this, $a, $b, TMP_5;
if (promise == null) promise = nil;
      return self.handler = ($a = ($b = self).$proc, $a.$$p = (TMP_5 = function(res){var self = TMP_5.$$s || this, $a;
if (res == null) res = nil;
        if ((($a = res['$ok?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
            return promise.$resolve(res)
            } else {
            return promise.$reject(res)
          }}, TMP_5.$$s = self, TMP_5), $a).call($b)}, TMP_4.$$s = self, TMP_4), $a).call($b);
    };

    def.$succeed = function(data, status, xhr) {
      var $a, self = this;

      
      self.body = data;
      self.xhr  = xhr;
      self.status_code = xhr.status;

      if (typeof(data) === 'object') {
        self.json = $scope.get('JSON').$from_object(data);
      }
    ;
      if ((($a = self.handler) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.handler.$call(self)
        } else {
        return nil
      };
    };

    return (def.$fail = function(xhr, status, error) {
      var $a, self = this;

      
      self.body = xhr.responseText;
      self.xhr = xhr;
      self.status_code = xhr.status;
    ;
      self.ok = false;
      if ((($a = self.handler) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.handler.$call(self)
        } else {
        return nil
      };
    }, nil) && 'fail';
  })(self, null);
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery/kernel"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defn(self, '$alert', function(msg) {
      var self = this;

      alert(msg);
      return nil;
    })
  })(self)
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal/jquery"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$==', '$require']);
  if ($scope.get('RUBY_ENGINE')['$==']("opal")) {
    self.$require("opal/jquery/window");
    self.$require("opal/jquery/document");
    self.$require("opal/jquery/element");
    self.$require("opal/jquery/event");
    self.$require("opal/jquery/http");
    return self.$require("opal/jquery/kernel");}
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal-jquery"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  return self.$require("opal/jquery")
};
/* Generated by Opal 0.8.1 */
Opal.modules["opal_ujs"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("jquery");
  self.$require("jquery_ujs");
  self.$require("opal");
  return self.$require("opal-jquery");
};
// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, vendor/assets/javascripts,
// or any plugin's vendor/assets/javascripts directory can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// compiled file.
//
// Read Sprockets README (https://github.com/rails/sprockets#sprockets-directives) for details
// about supported directives.
//







;
(function() {
  var CSRFToken, Click, ComponentUrl, EVENTS, Link, ProgressBar, browserIsntBuggy, browserSupportsCustomEvents, browserSupportsPushState, browserSupportsTurbolinks, bypassOnLoadPopstate, cacheCurrentPage, cacheSize, changePage, clone, constrainPageCacheTo, createDocument, crossOriginRedirect, currentState, enableProgressBar, enableTransitionCache, executeScriptTags, extractTitleAndBody, fetch, fetchHistory, fetchReplacement, historyStateIsDefined, initializeTurbolinks, installDocumentReadyPageEventTriggers, installHistoryChangeHandler, installJqueryAjaxSuccessPageUpdateTrigger, loadedAssets, manuallyTriggerHashChangeForFirefox, pageCache, pageChangePrevented, pagesCached, popCookie, processResponse, progressBar, recallScrollPosition, ref, referer, reflectNewUrl, reflectRedirectedUrl, rememberCurrentState, rememberCurrentUrl, rememberReferer, removeNoscriptTags, requestMethodIsSafe, resetScrollPosition, setAutofocusElement, transitionCacheEnabled, transitionCacheFor, triggerEvent, visit, xhr,
    indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; },
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty,
    slice = [].slice,
    bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

  pageCache = {};

  cacheSize = 10;

  transitionCacheEnabled = false;

  progressBar = null;

  currentState = null;

  loadedAssets = null;

  referer = null;

  xhr = null;

  EVENTS = {
    BEFORE_CHANGE: 'page:before-change',
    FETCH: 'page:fetch',
    RECEIVE: 'page:receive',
    CHANGE: 'page:change',
    UPDATE: 'page:update',
    LOAD: 'page:load',
    RESTORE: 'page:restore',
    BEFORE_UNLOAD: 'page:before-unload',
    EXPIRE: 'page:expire'
  };

  fetch = function(url) {
    var cachedPage;
    url = new ComponentUrl(url);
    rememberReferer();
    cacheCurrentPage();
    if (progressBar != null) {
      progressBar.start();
    }
    if (transitionCacheEnabled && (cachedPage = transitionCacheFor(url.absolute))) {
      fetchHistory(cachedPage);
      return fetchReplacement(url, null, false);
    } else {
      return fetchReplacement(url, resetScrollPosition);
    }
  };

  transitionCacheFor = function(url) {
    var cachedPage;
    cachedPage = pageCache[url];
    if (cachedPage && !cachedPage.transitionCacheDisabled) {
      return cachedPage;
    }
  };

  enableTransitionCache = function(enable) {
    if (enable == null) {
      enable = true;
    }
    return transitionCacheEnabled = enable;
  };

  enableProgressBar = function(enable) {
    if (enable == null) {
      enable = true;
    }
    if (!browserSupportsTurbolinks) {
      return;
    }
    if (enable) {
      return progressBar != null ? progressBar : progressBar = new ProgressBar('html');
    } else {
      if (progressBar != null) {
        progressBar.uninstall();
      }
      return progressBar = null;
    }
  };

  fetchReplacement = function(url, onLoadFunction, showProgressBar) {
    if (showProgressBar == null) {
      showProgressBar = true;
    }
    triggerEvent(EVENTS.FETCH, {
      url: url.absolute
    });
    if (xhr != null) {
      xhr.abort();
    }
    xhr = new XMLHttpRequest;
    xhr.open('GET', url.withoutHashForIE10compatibility(), true);
    xhr.setRequestHeader('Accept', 'text/html, application/xhtml+xml, application/xml');
    xhr.setRequestHeader('X-XHR-Referer', referer);
    xhr.onload = function() {
      var doc;
      triggerEvent(EVENTS.RECEIVE, {
        url: url.absolute
      });
      if (doc = processResponse()) {
        reflectNewUrl(url);
        reflectRedirectedUrl();
        changePage.apply(null, extractTitleAndBody(doc));
        manuallyTriggerHashChangeForFirefox();
        if (typeof onLoadFunction === "function") {
          onLoadFunction();
        }
        return triggerEvent(EVENTS.LOAD);
      } else {
        return document.location.href = crossOriginRedirect() || url.absolute;
      }
    };
    if (progressBar && showProgressBar) {
      xhr.onprogress = (function(_this) {
        return function(event) {
          var percent;
          percent = event.lengthComputable ? event.loaded / event.total * 100 : progressBar.value + (100 - progressBar.value) / 10;
          return progressBar.advanceTo(percent);
        };
      })(this);
    }
    xhr.onloadend = function() {
      return xhr = null;
    };
    xhr.onerror = function() {
      return document.location.href = url.absolute;
    };
    return xhr.send();
  };

  fetchHistory = function(cachedPage) {
    if (xhr != null) {
      xhr.abort();
    }
    changePage(cachedPage.title, cachedPage.body);
    recallScrollPosition(cachedPage);
    return triggerEvent(EVENTS.RESTORE);
  };

  cacheCurrentPage = function() {
    var currentStateUrl;
    currentStateUrl = new ComponentUrl(currentState.url);
    pageCache[currentStateUrl.absolute] = {
      url: currentStateUrl.relative,
      body: document.body,
      title: document.title,
      positionY: window.pageYOffset,
      positionX: window.pageXOffset,
      cachedAt: new Date().getTime(),
      transitionCacheDisabled: document.querySelector('[data-no-transition-cache]') != null
    };
    return constrainPageCacheTo(cacheSize);
  };

  pagesCached = function(size) {
    if (size == null) {
      size = cacheSize;
    }
    if (/^[\d]+$/.test(size)) {
      return cacheSize = parseInt(size);
    }
  };

  constrainPageCacheTo = function(limit) {
    var cacheTimesRecentFirst, i, key, len, pageCacheKeys, results;
    pageCacheKeys = Object.keys(pageCache);
    cacheTimesRecentFirst = pageCacheKeys.map(function(url) {
      return pageCache[url].cachedAt;
    }).sort(function(a, b) {
      return b - a;
    });
    results = [];
    for (i = 0, len = pageCacheKeys.length; i < len; i++) {
      key = pageCacheKeys[i];
      if (!(pageCache[key].cachedAt <= cacheTimesRecentFirst[limit])) {
        continue;
      }
      triggerEvent(EVENTS.EXPIRE, pageCache[key]);
      results.push(delete pageCache[key]);
    }
    return results;
  };

  changePage = function(title, body, csrfToken, runScripts) {
    triggerEvent(EVENTS.BEFORE_UNLOAD);
    document.title = title;
    document.documentElement.replaceChild(body, document.body);
    if (csrfToken != null) {
      CSRFToken.update(csrfToken);
    }
    setAutofocusElement();
    if (runScripts) {
      executeScriptTags();
    }
    currentState = window.history.state;
    if (progressBar != null) {
      progressBar.done();
    }
    triggerEvent(EVENTS.CHANGE);
    return triggerEvent(EVENTS.UPDATE);
  };

  executeScriptTags = function() {
    var attr, copy, i, j, len, len1, nextSibling, parentNode, ref, ref1, script, scripts;
    scripts = Array.prototype.slice.call(document.body.querySelectorAll('script:not([data-turbolinks-eval="false"])'));
    for (i = 0, len = scripts.length; i < len; i++) {
      script = scripts[i];
      if (!((ref = script.type) === '' || ref === 'text/javascript')) {
        continue;
      }
      copy = document.createElement('script');
      ref1 = script.attributes;
      for (j = 0, len1 = ref1.length; j < len1; j++) {
        attr = ref1[j];
        copy.setAttribute(attr.name, attr.value);
      }
      if (!script.hasAttribute('async')) {
        copy.async = false;
      }
      copy.appendChild(document.createTextNode(script.innerHTML));
      parentNode = script.parentNode, nextSibling = script.nextSibling;
      parentNode.removeChild(script);
      parentNode.insertBefore(copy, nextSibling);
    }
  };

  removeNoscriptTags = function(node) {
    node.innerHTML = node.innerHTML.replace(/<noscript[\S\s]*?<\/noscript>/ig, '');
    return node;
  };

  setAutofocusElement = function() {
    var autofocusElement, list;
    autofocusElement = (list = document.querySelectorAll('input[autofocus], textarea[autofocus]'))[list.length - 1];
    if (autofocusElement && document.activeElement !== autofocusElement) {
      return autofocusElement.focus();
    }
  };

  reflectNewUrl = function(url) {
    if ((url = new ComponentUrl(url)).absolute !== referer) {
      return window.history.pushState({
        turbolinks: true,
        url: url.absolute
      }, '', url.absolute);
    }
  };

  reflectRedirectedUrl = function() {
    var location, preservedHash;
    if (location = xhr.getResponseHeader('X-XHR-Redirected-To')) {
      location = new ComponentUrl(location);
      preservedHash = location.hasNoHash() ? document.location.hash : '';
      return window.history.replaceState(window.history.state, '', location.href + preservedHash);
    }
  };

  crossOriginRedirect = function() {
    var redirect;
    if (((redirect = xhr.getResponseHeader('Location')) != null) && (new ComponentUrl(redirect)).crossOrigin()) {
      return redirect;
    }
  };

  rememberReferer = function() {
    return referer = document.location.href;
  };

  rememberCurrentUrl = function() {
    return window.history.replaceState({
      turbolinks: true,
      url: document.location.href
    }, '', document.location.href);
  };

  rememberCurrentState = function() {
    return currentState = window.history.state;
  };

  manuallyTriggerHashChangeForFirefox = function() {
    var url;
    if (navigator.userAgent.match(/Firefox/) && !(url = new ComponentUrl).hasNoHash()) {
      window.history.replaceState(currentState, '', url.withoutHash());
      return document.location.hash = url.hash;
    }
  };

  recallScrollPosition = function(page) {
    return window.scrollTo(page.positionX, page.positionY);
  };

  resetScrollPosition = function() {
    if (document.location.hash) {
      return document.location.href = document.location.href;
    } else {
      return window.scrollTo(0, 0);
    }
  };

  clone = function(original) {
    var copy, key, value;
    if ((original == null) || typeof original !== 'object') {
      return original;
    }
    copy = new original.constructor();
    for (key in original) {
      value = original[key];
      copy[key] = clone(value);
    }
    return copy;
  };

  popCookie = function(name) {
    var ref, value;
    value = ((ref = document.cookie.match(new RegExp(name + "=(\\w+)"))) != null ? ref[1].toUpperCase() : void 0) || '';
    document.cookie = name + '=; expires=Thu, 01-Jan-70 00:00:01 GMT; path=/';
    return value;
  };

  triggerEvent = function(name, data) {
    var event;
    if (typeof Prototype !== 'undefined') {
      Event.fire(document, name, data, true);
    }
    event = document.createEvent('Events');
    if (data) {
      event.data = data;
    }
    event.initEvent(name, true, true);
    return document.dispatchEvent(event);
  };

  pageChangePrevented = function(url) {
    return !triggerEvent(EVENTS.BEFORE_CHANGE, {
      url: url
    });
  };

  processResponse = function() {
    var assetsChanged, clientOrServerError, doc, extractTrackAssets, intersection, validContent;
    clientOrServerError = function() {
      var ref;
      return (400 <= (ref = xhr.status) && ref < 600);
    };
    validContent = function() {
      var contentType;
      return ((contentType = xhr.getResponseHeader('Content-Type')) != null) && contentType.match(/^(?:text\/html|application\/xhtml\+xml|application\/xml)(?:;|$)/);
    };
    extractTrackAssets = function(doc) {
      var i, len, node, ref, results;
      ref = doc.querySelector('head').childNodes;
      results = [];
      for (i = 0, len = ref.length; i < len; i++) {
        node = ref[i];
        if ((typeof node.getAttribute === "function" ? node.getAttribute('data-turbolinks-track') : void 0) != null) {
          results.push(node.getAttribute('src') || node.getAttribute('href'));
        }
      }
      return results;
    };
    assetsChanged = function(doc) {
      var fetchedAssets;
      loadedAssets || (loadedAssets = extractTrackAssets(document));
      fetchedAssets = extractTrackAssets(doc);
      return fetchedAssets.length !== loadedAssets.length || intersection(fetchedAssets, loadedAssets).length !== loadedAssets.length;
    };
    intersection = function(a, b) {
      var i, len, ref, results, value;
      if (a.length > b.length) {
        ref = [b, a], a = ref[0], b = ref[1];
      }
      results = [];
      for (i = 0, len = a.length; i < len; i++) {
        value = a[i];
        if (indexOf.call(b, value) >= 0) {
          results.push(value);
        }
      }
      return results;
    };
    if (!clientOrServerError() && validContent()) {
      doc = createDocument(xhr.responseText);
      if (doc && !assetsChanged(doc)) {
        return doc;
      }
    }
  };

  extractTitleAndBody = function(doc) {
    var title;
    title = doc.querySelector('title');
    return [title != null ? title.textContent : void 0, removeNoscriptTags(doc.querySelector('body')), CSRFToken.get(doc).token, 'runScripts'];
  };

  CSRFToken = {
    get: function(doc) {
      var tag;
      if (doc == null) {
        doc = document;
      }
      return {
        node: tag = doc.querySelector('meta[name="csrf-token"]'),
        token: tag != null ? typeof tag.getAttribute === "function" ? tag.getAttribute('content') : void 0 : void 0
      };
    },
    update: function(latest) {
      var current;
      current = this.get();
      if ((current.token != null) && (latest != null) && current.token !== latest) {
        return current.node.setAttribute('content', latest);
      }
    }
  };

  createDocument = function(html) {
    var doc;
    doc = document.documentElement.cloneNode();
    doc.innerHTML = html;
    doc.head = doc.querySelector('head');
    doc.body = doc.querySelector('body');
    return doc;
  };

  ComponentUrl = (function() {
    function ComponentUrl(original1) {
      this.original = original1 != null ? original1 : document.location.href;
      if (this.original.constructor === ComponentUrl) {
        return this.original;
      }
      this._parse();
    }

    ComponentUrl.prototype.withoutHash = function() {
      return this.href.replace(this.hash, '').replace('#', '');
    };

    ComponentUrl.prototype.withoutHashForIE10compatibility = function() {
      return this.withoutHash();
    };

    ComponentUrl.prototype.hasNoHash = function() {
      return this.hash.length === 0;
    };

    ComponentUrl.prototype.crossOrigin = function() {
      return this.origin !== (new ComponentUrl).origin;
    };

    ComponentUrl.prototype._parse = function() {
      var ref;
      (this.link != null ? this.link : this.link = document.createElement('a')).href = this.original;
      ref = this.link, this.href = ref.href, this.protocol = ref.protocol, this.host = ref.host, this.hostname = ref.hostname, this.port = ref.port, this.pathname = ref.pathname, this.search = ref.search, this.hash = ref.hash;
      this.origin = [this.protocol, '//', this.hostname].join('');
      if (this.port.length !== 0) {
        this.origin += ":" + this.port;
      }
      this.relative = [this.pathname, this.search, this.hash].join('');
      return this.absolute = this.href;
    };

    return ComponentUrl;

  })();

  Link = (function(superClass) {
    extend(Link, superClass);

    Link.HTML_EXTENSIONS = ['html'];

    Link.allowExtensions = function() {
      var extension, extensions, i, len;
      extensions = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      for (i = 0, len = extensions.length; i < len; i++) {
        extension = extensions[i];
        Link.HTML_EXTENSIONS.push(extension);
      }
      return Link.HTML_EXTENSIONS;
    };

    function Link(link1) {
      this.link = link1;
      if (this.link.constructor === Link) {
        return this.link;
      }
      this.original = this.link.href;
      this.originalElement = this.link;
      this.link = this.link.cloneNode(false);
      Link.__super__.constructor.apply(this, arguments);
    }

    Link.prototype.shouldIgnore = function() {
      return this.crossOrigin() || this._anchored() || this._nonHtml() || this._optOut() || this._target();
    };

    Link.prototype._anchored = function() {
      return (this.hash.length > 0 || this.href.charAt(this.href.length - 1) === '#') && (this.withoutHash() === (new ComponentUrl).withoutHash());
    };

    Link.prototype._nonHtml = function() {
      return this.pathname.match(/\.[a-z]+$/g) && !this.pathname.match(new RegExp("\\.(?:" + (Link.HTML_EXTENSIONS.join('|')) + ")?$", 'g'));
    };

    Link.prototype._optOut = function() {
      var ignore, link;
      link = this.originalElement;
      while (!(ignore || link === document)) {
        ignore = link.getAttribute('data-no-turbolink') != null;
        link = link.parentNode;
      }
      return ignore;
    };

    Link.prototype._target = function() {
      return this.link.target.length !== 0;
    };

    return Link;

  })(ComponentUrl);

  Click = (function() {
    Click.installHandlerLast = function(event) {
      if (!event.defaultPrevented) {
        document.removeEventListener('click', Click.handle, false);
        return document.addEventListener('click', Click.handle, false);
      }
    };

    Click.handle = function(event) {
      return new Click(event);
    };

    function Click(event1) {
      this.event = event1;
      if (this.event.defaultPrevented) {
        return;
      }
      this._extractLink();
      if (this._validForTurbolinks()) {
        if (!pageChangePrevented(this.link.absolute)) {
          visit(this.link.href);
        }
        this.event.preventDefault();
      }
    }

    Click.prototype._extractLink = function() {
      var link;
      link = this.event.target;
      while (!(!link.parentNode || link.nodeName === 'A')) {
        link = link.parentNode;
      }
      if (link.nodeName === 'A' && link.href.length !== 0) {
        return this.link = new Link(link);
      }
    };

    Click.prototype._validForTurbolinks = function() {
      return (this.link != null) && !(this.link.shouldIgnore() || this._nonStandardClick());
    };

    Click.prototype._nonStandardClick = function() {
      return this.event.which > 1 || this.event.metaKey || this.event.ctrlKey || this.event.shiftKey || this.event.altKey;
    };

    return Click;

  })();

  ProgressBar = (function() {
    var className;

    className = 'turbolinks-progress-bar';

    function ProgressBar(elementSelector) {
      this.elementSelector = elementSelector;
      this._trickle = bind(this._trickle, this);
      this.value = 0;
      this.content = '';
      this.speed = 300;
      this.opacity = 0.99;
      this.install();
    }

    ProgressBar.prototype.install = function() {
      this.element = document.querySelector(this.elementSelector);
      this.element.classList.add(className);
      this.styleElement = document.createElement('style');
      document.head.appendChild(this.styleElement);
      return this._updateStyle();
    };

    ProgressBar.prototype.uninstall = function() {
      this.element.classList.remove(className);
      return document.head.removeChild(this.styleElement);
    };

    ProgressBar.prototype.start = function() {
      return this.advanceTo(5);
    };

    ProgressBar.prototype.advanceTo = function(value) {
      var ref;
      if ((value > (ref = this.value) && ref <= 100)) {
        this.value = value;
        this._updateStyle();
        if (this.value === 100) {
          return this._stopTrickle();
        } else if (this.value > 0) {
          return this._startTrickle();
        }
      }
    };

    ProgressBar.prototype.done = function() {
      if (this.value > 0) {
        this.advanceTo(100);
        return this._reset();
      }
    };

    ProgressBar.prototype._reset = function() {
      var originalOpacity;
      originalOpacity = this.opacity;
      setTimeout((function(_this) {
        return function() {
          _this.opacity = 0;
          return _this._updateStyle();
        };
      })(this), this.speed / 2);
      return setTimeout((function(_this) {
        return function() {
          _this.value = 0;
          _this.opacity = originalOpacity;
          return _this._withSpeed(0, function() {
            return _this._updateStyle(true);
          });
        };
      })(this), this.speed);
    };

    ProgressBar.prototype._startTrickle = function() {
      if (this.trickling) {
        return;
      }
      this.trickling = true;
      return setTimeout(this._trickle, this.speed);
    };

    ProgressBar.prototype._stopTrickle = function() {
      return delete this.trickling;
    };

    ProgressBar.prototype._trickle = function() {
      if (!this.trickling) {
        return;
      }
      this.advanceTo(this.value + Math.random() / 2);
      return setTimeout(this._trickle, this.speed);
    };

    ProgressBar.prototype._withSpeed = function(speed, fn) {
      var originalSpeed, result;
      originalSpeed = this.speed;
      this.speed = speed;
      result = fn();
      this.speed = originalSpeed;
      return result;
    };

    ProgressBar.prototype._updateStyle = function(forceRepaint) {
      if (forceRepaint == null) {
        forceRepaint = false;
      }
      if (forceRepaint) {
        this._changeContentToForceRepaint();
      }
      return this.styleElement.textContent = this._createCSSRule();
    };

    ProgressBar.prototype._changeContentToForceRepaint = function() {
      return this.content = this.content === '' ? ' ' : '';
    };

    ProgressBar.prototype._createCSSRule = function() {
      return this.elementSelector + "." + className + "::before {\n  content: '" + this.content + "';\n  position: fixed;\n  top: 0;\n  left: 0;\n  z-index: 2000;\n  background-color: #0076ff;\n  height: 3px;\n  opacity: " + this.opacity + ";\n  width: " + this.value + "%;\n  transition: width " + this.speed + "ms ease-out, opacity " + (this.speed / 2) + "ms ease-in;\n  transform: translate3d(0,0,0);\n}";
    };

    return ProgressBar;

  })();

  bypassOnLoadPopstate = function(fn) {
    return setTimeout(fn, 500);
  };

  installDocumentReadyPageEventTriggers = function() {
    return document.addEventListener('DOMContentLoaded', (function() {
      triggerEvent(EVENTS.CHANGE);
      return triggerEvent(EVENTS.UPDATE);
    }), true);
  };

  installJqueryAjaxSuccessPageUpdateTrigger = function() {
    if (typeof jQuery !== 'undefined') {
      return jQuery(document).on('ajaxSuccess', function(event, xhr, settings) {
        if (!jQuery.trim(xhr.responseText)) {
          return;
        }
        return triggerEvent(EVENTS.UPDATE);
      });
    }
  };

  installHistoryChangeHandler = function(event) {
    var cachedPage, ref;
    if ((ref = event.state) != null ? ref.turbolinks : void 0) {
      if (cachedPage = pageCache[(new ComponentUrl(event.state.url)).absolute]) {
        cacheCurrentPage();
        return fetchHistory(cachedPage);
      } else {
        return visit(event.target.location.href);
      }
    }
  };

  initializeTurbolinks = function() {
    rememberCurrentUrl();
    rememberCurrentState();
    document.addEventListener('click', Click.installHandlerLast, true);
    window.addEventListener('hashchange', function(event) {
      rememberCurrentUrl();
      return rememberCurrentState();
    }, false);
    return bypassOnLoadPopstate(function() {
      return window.addEventListener('popstate', installHistoryChangeHandler, false);
    });
  };

  historyStateIsDefined = window.history.state !== void 0 || navigator.userAgent.match(/Firefox\/2[6|7]/);

  browserSupportsPushState = window.history && window.history.pushState && window.history.replaceState && historyStateIsDefined;

  browserIsntBuggy = !navigator.userAgent.match(/CriOS\//);

  requestMethodIsSafe = (ref = popCookie('request_method')) === 'GET' || ref === '';

  browserSupportsTurbolinks = browserSupportsPushState && browserIsntBuggy && requestMethodIsSafe;

  browserSupportsCustomEvents = document.addEventListener && document.createEvent;

  if (browserSupportsCustomEvents) {
    installDocumentReadyPageEventTriggers();
    installJqueryAjaxSuccessPageUpdateTrigger();
  }

  if (browserSupportsTurbolinks) {
    visit = fetch;
    initializeTurbolinks();
  } else {
    visit = function(url) {
      return document.location.href = url;
    };
  }

  this.Turbolinks = {
    visit: visit,
    pagesCached: pagesCached,
    enableTransitionCache: enableTransitionCache,
    enableProgressBar: enableProgressBar,
    allowLinkExtensions: Link.allowExtensions,
    supported: browserSupportsTurbolinks,
    EVENTS: clone(EVENTS)
  };

}).call(this);
/*!
 * Bootstrap v3.3.6 (http://getbootstrap.com)
 * Copyright 2011-2015 Twitter, Inc.
 * Licensed under the MIT license
 */

if("undefined"==typeof jQuery)throw new Error("Bootstrap's JavaScript requires jQuery");+function(a){"use strict";var b=a.fn.jquery.split(" ")[0].split(".");if(b[0]<2&&b[1]<9||1==b[0]&&9==b[1]&&b[2]<1||b[0]>2)throw new Error("Bootstrap's JavaScript requires jQuery version 1.9.1 or higher, but lower than version 3")}(jQuery),+function(a){"use strict";function b(){var a=document.createElement("bootstrap"),b={WebkitTransition:"webkitTransitionEnd",MozTransition:"transitionend",OTransition:"oTransitionEnd otransitionend",transition:"transitionend"};for(var c in b)if(void 0!==a.style[c])return{end:b[c]};return!1}a.fn.emulateTransitionEnd=function(b){var c=!1,d=this;a(this).one("bsTransitionEnd",function(){c=!0});var e=function(){c||a(d).trigger(a.support.transition.end)};return setTimeout(e,b),this},a(function(){a.support.transition=b(),a.support.transition&&(a.event.special.bsTransitionEnd={bindType:a.support.transition.end,delegateType:a.support.transition.end,handle:function(b){return a(b.target).is(this)?b.handleObj.handler.apply(this,arguments):void 0}})})}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var c=a(this),e=c.data("bs.alert");e||c.data("bs.alert",e=new d(this)),"string"==typeof b&&e[b].call(c)})}var c='[data-dismiss="alert"]',d=function(b){a(b).on("click",c,this.close)};d.VERSION="3.3.6",d.TRANSITION_DURATION=150,d.prototype.close=function(b){function c(){g.detach().trigger("closed.bs.alert").remove()}var e=a(this),f=e.attr("data-target");f||(f=e.attr("href"),f=f&&f.replace(/.*(?=#[^\s]*$)/,""));var g=a(f);b&&b.preventDefault(),g.length||(g=e.closest(".alert")),g.trigger(b=a.Event("close.bs.alert")),b.isDefaultPrevented()||(g.removeClass("in"),a.support.transition&&g.hasClass("fade")?g.one("bsTransitionEnd",c).emulateTransitionEnd(d.TRANSITION_DURATION):c())};var e=a.fn.alert;a.fn.alert=b,a.fn.alert.Constructor=d,a.fn.alert.noConflict=function(){return a.fn.alert=e,this},a(document).on("click.bs.alert.data-api",c,d.prototype.close)}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var d=a(this),e=d.data("bs.button"),f="object"==typeof b&&b;e||d.data("bs.button",e=new c(this,f)),"toggle"==b?e.toggle():b&&e.setState(b)})}var c=function(b,d){this.$element=a(b),this.options=a.extend({},c.DEFAULTS,d),this.isLoading=!1};c.VERSION="3.3.6",c.DEFAULTS={loadingText:"loading..."},c.prototype.setState=function(b){var c="disabled",d=this.$element,e=d.is("input")?"val":"html",f=d.data();b+="Text",null==f.resetText&&d.data("resetText",d[e]()),setTimeout(a.proxy(function(){d[e](null==f[b]?this.options[b]:f[b]),"loadingText"==b?(this.isLoading=!0,d.addClass(c).attr(c,c)):this.isLoading&&(this.isLoading=!1,d.removeClass(c).removeAttr(c))},this),0)},c.prototype.toggle=function(){var a=!0,b=this.$element.closest('[data-toggle="buttons"]');if(b.length){var c=this.$element.find("input");"radio"==c.prop("type")?(c.prop("checked")&&(a=!1),b.find(".active").removeClass("active"),this.$element.addClass("active")):"checkbox"==c.prop("type")&&(c.prop("checked")!==this.$element.hasClass("active")&&(a=!1),this.$element.toggleClass("active")),c.prop("checked",this.$element.hasClass("active")),a&&c.trigger("change")}else this.$element.attr("aria-pressed",!this.$element.hasClass("active")),this.$element.toggleClass("active")};var d=a.fn.button;a.fn.button=b,a.fn.button.Constructor=c,a.fn.button.noConflict=function(){return a.fn.button=d,this},a(document).on("click.bs.button.data-api",'[data-toggle^="button"]',function(c){var d=a(c.target);d.hasClass("btn")||(d=d.closest(".btn")),b.call(d,"toggle"),a(c.target).is('input[type="radio"]')||a(c.target).is('input[type="checkbox"]')||c.preventDefault()}).on("focus.bs.button.data-api blur.bs.button.data-api",'[data-toggle^="button"]',function(b){a(b.target).closest(".btn").toggleClass("focus",/^focus(in)?$/.test(b.type))})}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var d=a(this),e=d.data("bs.carousel"),f=a.extend({},c.DEFAULTS,d.data(),"object"==typeof b&&b),g="string"==typeof b?b:f.slide;e||d.data("bs.carousel",e=new c(this,f)),"number"==typeof b?e.to(b):g?e[g]():f.interval&&e.pause().cycle()})}var c=function(b,c){this.$element=a(b),this.$indicators=this.$element.find(".carousel-indicators"),this.options=c,this.paused=null,this.sliding=null,this.interval=null,this.$active=null,this.$items=null,this.options.keyboard&&this.$element.on("keydown.bs.carousel",a.proxy(this.keydown,this)),"hover"==this.options.pause&&!("ontouchstart"in document.documentElement)&&this.$element.on("mouseenter.bs.carousel",a.proxy(this.pause,this)).on("mouseleave.bs.carousel",a.proxy(this.cycle,this))};c.VERSION="3.3.6",c.TRANSITION_DURATION=600,c.DEFAULTS={interval:5e3,pause:"hover",wrap:!0,keyboard:!0},c.prototype.keydown=function(a){if(!/input|textarea/i.test(a.target.tagName)){switch(a.which){case 37:this.prev();break;case 39:this.next();break;default:return}a.preventDefault()}},c.prototype.cycle=function(b){return b||(this.paused=!1),this.interval&&clearInterval(this.interval),this.options.interval&&!this.paused&&(this.interval=setInterval(a.proxy(this.next,this),this.options.interval)),this},c.prototype.getItemIndex=function(a){return this.$items=a.parent().children(".item"),this.$items.index(a||this.$active)},c.prototype.getItemForDirection=function(a,b){var c=this.getItemIndex(b),d="prev"==a&&0===c||"next"==a&&c==this.$items.length-1;if(d&&!this.options.wrap)return b;var e="prev"==a?-1:1,f=(c+e)%this.$items.length;return this.$items.eq(f)},c.prototype.to=function(a){var b=this,c=this.getItemIndex(this.$active=this.$element.find(".item.active"));return a>this.$items.length-1||0>a?void 0:this.sliding?this.$element.one("slid.bs.carousel",function(){b.to(a)}):c==a?this.pause().cycle():this.slide(a>c?"next":"prev",this.$items.eq(a))},c.prototype.pause=function(b){return b||(this.paused=!0),this.$element.find(".next, .prev").length&&a.support.transition&&(this.$element.trigger(a.support.transition.end),this.cycle(!0)),this.interval=clearInterval(this.interval),this},c.prototype.next=function(){return this.sliding?void 0:this.slide("next")},c.prototype.prev=function(){return this.sliding?void 0:this.slide("prev")},c.prototype.slide=function(b,d){var e=this.$element.find(".item.active"),f=d||this.getItemForDirection(b,e),g=this.interval,h="next"==b?"left":"right",i=this;if(f.hasClass("active"))return this.sliding=!1;var j=f[0],k=a.Event("slide.bs.carousel",{relatedTarget:j,direction:h});if(this.$element.trigger(k),!k.isDefaultPrevented()){if(this.sliding=!0,g&&this.pause(),this.$indicators.length){this.$indicators.find(".active").removeClass("active");var l=a(this.$indicators.children()[this.getItemIndex(f)]);l&&l.addClass("active")}var m=a.Event("slid.bs.carousel",{relatedTarget:j,direction:h});return a.support.transition&&this.$element.hasClass("slide")?(f.addClass(b),f[0].offsetWidth,e.addClass(h),f.addClass(h),e.one("bsTransitionEnd",function(){f.removeClass([b,h].join(" ")).addClass("active"),e.removeClass(["active",h].join(" ")),i.sliding=!1,setTimeout(function(){i.$element.trigger(m)},0)}).emulateTransitionEnd(c.TRANSITION_DURATION)):(e.removeClass("active"),f.addClass("active"),this.sliding=!1,this.$element.trigger(m)),g&&this.cycle(),this}};var d=a.fn.carousel;a.fn.carousel=b,a.fn.carousel.Constructor=c,a.fn.carousel.noConflict=function(){return a.fn.carousel=d,this};var e=function(c){var d,e=a(this),f=a(e.attr("data-target")||(d=e.attr("href"))&&d.replace(/.*(?=#[^\s]+$)/,""));if(f.hasClass("carousel")){var g=a.extend({},f.data(),e.data()),h=e.attr("data-slide-to");h&&(g.interval=!1),b.call(f,g),h&&f.data("bs.carousel").to(h),c.preventDefault()}};a(document).on("click.bs.carousel.data-api","[data-slide]",e).on("click.bs.carousel.data-api","[data-slide-to]",e),a(window).on("load",function(){a('[data-ride="carousel"]').each(function(){var c=a(this);b.call(c,c.data())})})}(jQuery),+function(a){"use strict";function b(b){var c,d=b.attr("data-target")||(c=b.attr("href"))&&c.replace(/.*(?=#[^\s]+$)/,"");return a(d)}function c(b){return this.each(function(){var c=a(this),e=c.data("bs.collapse"),f=a.extend({},d.DEFAULTS,c.data(),"object"==typeof b&&b);!e&&f.toggle&&/show|hide/.test(b)&&(f.toggle=!1),e||c.data("bs.collapse",e=new d(this,f)),"string"==typeof b&&e[b]()})}var d=function(b,c){this.$element=a(b),this.options=a.extend({},d.DEFAULTS,c),this.$trigger=a('[data-toggle="collapse"][href="#'+b.id+'"],[data-toggle="collapse"][data-target="#'+b.id+'"]'),this.transitioning=null,this.options.parent?this.$parent=this.getParent():this.addAriaAndCollapsedClass(this.$element,this.$trigger),this.options.toggle&&this.toggle()};d.VERSION="3.3.6",d.TRANSITION_DURATION=350,d.DEFAULTS={toggle:!0},d.prototype.dimension=function(){var a=this.$element.hasClass("width");return a?"width":"height"},d.prototype.show=function(){if(!this.transitioning&&!this.$element.hasClass("in")){var b,e=this.$parent&&this.$parent.children(".panel").children(".in, .collapsing");if(!(e&&e.length&&(b=e.data("bs.collapse"),b&&b.transitioning))){var f=a.Event("show.bs.collapse");if(this.$element.trigger(f),!f.isDefaultPrevented()){e&&e.length&&(c.call(e,"hide"),b||e.data("bs.collapse",null));var g=this.dimension();this.$element.removeClass("collapse").addClass("collapsing")[g](0).attr("aria-expanded",!0),this.$trigger.removeClass("collapsed").attr("aria-expanded",!0),this.transitioning=1;var h=function(){this.$element.removeClass("collapsing").addClass("collapse in")[g](""),this.transitioning=0,this.$element.trigger("shown.bs.collapse")};if(!a.support.transition)return h.call(this);var i=a.camelCase(["scroll",g].join("-"));this.$element.one("bsTransitionEnd",a.proxy(h,this)).emulateTransitionEnd(d.TRANSITION_DURATION)[g](this.$element[0][i])}}}},d.prototype.hide=function(){if(!this.transitioning&&this.$element.hasClass("in")){var b=a.Event("hide.bs.collapse");if(this.$element.trigger(b),!b.isDefaultPrevented()){var c=this.dimension();this.$element[c](this.$element[c]())[0].offsetHeight,this.$element.addClass("collapsing").removeClass("collapse in").attr("aria-expanded",!1),this.$trigger.addClass("collapsed").attr("aria-expanded",!1),this.transitioning=1;var e=function(){this.transitioning=0,this.$element.removeClass("collapsing").addClass("collapse").trigger("hidden.bs.collapse")};return a.support.transition?void this.$element[c](0).one("bsTransitionEnd",a.proxy(e,this)).emulateTransitionEnd(d.TRANSITION_DURATION):e.call(this)}}},d.prototype.toggle=function(){this[this.$element.hasClass("in")?"hide":"show"]()},d.prototype.getParent=function(){return a(this.options.parent).find('[data-toggle="collapse"][data-parent="'+this.options.parent+'"]').each(a.proxy(function(c,d){var e=a(d);this.addAriaAndCollapsedClass(b(e),e)},this)).end()},d.prototype.addAriaAndCollapsedClass=function(a,b){var c=a.hasClass("in");a.attr("aria-expanded",c),b.toggleClass("collapsed",!c).attr("aria-expanded",c)};var e=a.fn.collapse;a.fn.collapse=c,a.fn.collapse.Constructor=d,a.fn.collapse.noConflict=function(){return a.fn.collapse=e,this},a(document).on("click.bs.collapse.data-api",'[data-toggle="collapse"]',function(d){var e=a(this);e.attr("data-target")||d.preventDefault();var f=b(e),g=f.data("bs.collapse"),h=g?"toggle":e.data();c.call(f,h)})}(jQuery),+function(a){"use strict";function b(b){var c=b.attr("data-target");c||(c=b.attr("href"),c=c&&/#[A-Za-z]/.test(c)&&c.replace(/.*(?=#[^\s]*$)/,""));var d=c&&a(c);return d&&d.length?d:b.parent()}function c(c){c&&3===c.which||(a(e).remove(),a(f).each(function(){var d=a(this),e=b(d),f={relatedTarget:this};e.hasClass("open")&&(c&&"click"==c.type&&/input|textarea/i.test(c.target.tagName)&&a.contains(e[0],c.target)||(e.trigger(c=a.Event("hide.bs.dropdown",f)),c.isDefaultPrevented()||(d.attr("aria-expanded","false"),e.removeClass("open").trigger(a.Event("hidden.bs.dropdown",f)))))}))}function d(b){return this.each(function(){var c=a(this),d=c.data("bs.dropdown");d||c.data("bs.dropdown",d=new g(this)),"string"==typeof b&&d[b].call(c)})}var e=".dropdown-backdrop",f='[data-toggle="dropdown"]',g=function(b){a(b).on("click.bs.dropdown",this.toggle)};g.VERSION="3.3.6",g.prototype.toggle=function(d){var e=a(this);if(!e.is(".disabled, :disabled")){var f=b(e),g=f.hasClass("open");if(c(),!g){"ontouchstart"in document.documentElement&&!f.closest(".navbar-nav").length&&a(document.createElement("div")).addClass("dropdown-backdrop").insertAfter(a(this)).on("click",c);var h={relatedTarget:this};if(f.trigger(d=a.Event("show.bs.dropdown",h)),d.isDefaultPrevented())return;e.trigger("focus").attr("aria-expanded","true"),f.toggleClass("open").trigger(a.Event("shown.bs.dropdown",h))}return!1}},g.prototype.keydown=function(c){if(/(38|40|27|32)/.test(c.which)&&!/input|textarea/i.test(c.target.tagName)){var d=a(this);if(c.preventDefault(),c.stopPropagation(),!d.is(".disabled, :disabled")){var e=b(d),g=e.hasClass("open");if(!g&&27!=c.which||g&&27==c.which)return 27==c.which&&e.find(f).trigger("focus"),d.trigger("click");var h=" li:not(.disabled):visible a",i=e.find(".dropdown-menu"+h);if(i.length){var j=i.index(c.target);38==c.which&&j>0&&j--,40==c.which&&j<i.length-1&&j++,~j||(j=0),i.eq(j).trigger("focus")}}}};var h=a.fn.dropdown;a.fn.dropdown=d,a.fn.dropdown.Constructor=g,a.fn.dropdown.noConflict=function(){return a.fn.dropdown=h,this},a(document).on("click.bs.dropdown.data-api",c).on("click.bs.dropdown.data-api",".dropdown form",function(a){a.stopPropagation()}).on("click.bs.dropdown.data-api",f,g.prototype.toggle).on("keydown.bs.dropdown.data-api",f,g.prototype.keydown).on("keydown.bs.dropdown.data-api",".dropdown-menu",g.prototype.keydown)}(jQuery),+function(a){"use strict";function b(b,d){return this.each(function(){var e=a(this),f=e.data("bs.modal"),g=a.extend({},c.DEFAULTS,e.data(),"object"==typeof b&&b);f||e.data("bs.modal",f=new c(this,g)),"string"==typeof b?f[b](d):g.show&&f.show(d)})}var c=function(b,c){this.options=c,this.$body=a(document.body),this.$element=a(b),this.$dialog=this.$element.find(".modal-dialog"),this.$backdrop=null,this.isShown=null,this.originalBodyPad=null,this.scrollbarWidth=0,this.ignoreBackdropClick=!1,this.options.remote&&this.$element.find(".modal-content").load(this.options.remote,a.proxy(function(){this.$element.trigger("loaded.bs.modal")},this))};c.VERSION="3.3.6",c.TRANSITION_DURATION=300,c.BACKDROP_TRANSITION_DURATION=150,c.DEFAULTS={backdrop:!0,keyboard:!0,show:!0},c.prototype.toggle=function(a){return this.isShown?this.hide():this.show(a)},c.prototype.show=function(b){var d=this,e=a.Event("show.bs.modal",{relatedTarget:b});this.$element.trigger(e),this.isShown||e.isDefaultPrevented()||(this.isShown=!0,this.checkScrollbar(),this.setScrollbar(),this.$body.addClass("modal-open"),this.escape(),this.resize(),this.$element.on("click.dismiss.bs.modal",'[data-dismiss="modal"]',a.proxy(this.hide,this)),this.$dialog.on("mousedown.dismiss.bs.modal",function(){d.$element.one("mouseup.dismiss.bs.modal",function(b){a(b.target).is(d.$element)&&(d.ignoreBackdropClick=!0)})}),this.backdrop(function(){var e=a.support.transition&&d.$element.hasClass("fade");d.$element.parent().length||d.$element.appendTo(d.$body),d.$element.show().scrollTop(0),d.adjustDialog(),e&&d.$element[0].offsetWidth,d.$element.addClass("in"),d.enforceFocus();var f=a.Event("shown.bs.modal",{relatedTarget:b});e?d.$dialog.one("bsTransitionEnd",function(){d.$element.trigger("focus").trigger(f)}).emulateTransitionEnd(c.TRANSITION_DURATION):d.$element.trigger("focus").trigger(f)}))},c.prototype.hide=function(b){b&&b.preventDefault(),b=a.Event("hide.bs.modal"),this.$element.trigger(b),this.isShown&&!b.isDefaultPrevented()&&(this.isShown=!1,this.escape(),this.resize(),a(document).off("focusin.bs.modal"),this.$element.removeClass("in").off("click.dismiss.bs.modal").off("mouseup.dismiss.bs.modal"),this.$dialog.off("mousedown.dismiss.bs.modal"),a.support.transition&&this.$element.hasClass("fade")?this.$element.one("bsTransitionEnd",a.proxy(this.hideModal,this)).emulateTransitionEnd(c.TRANSITION_DURATION):this.hideModal())},c.prototype.enforceFocus=function(){a(document).off("focusin.bs.modal").on("focusin.bs.modal",a.proxy(function(a){this.$element[0]===a.target||this.$element.has(a.target).length||this.$element.trigger("focus")},this))},c.prototype.escape=function(){this.isShown&&this.options.keyboard?this.$element.on("keydown.dismiss.bs.modal",a.proxy(function(a){27==a.which&&this.hide()},this)):this.isShown||this.$element.off("keydown.dismiss.bs.modal")},c.prototype.resize=function(){this.isShown?a(window).on("resize.bs.modal",a.proxy(this.handleUpdate,this)):a(window).off("resize.bs.modal")},c.prototype.hideModal=function(){var a=this;this.$element.hide(),this.backdrop(function(){a.$body.removeClass("modal-open"),a.resetAdjustments(),a.resetScrollbar(),a.$element.trigger("hidden.bs.modal")})},c.prototype.removeBackdrop=function(){this.$backdrop&&this.$backdrop.remove(),this.$backdrop=null},c.prototype.backdrop=function(b){var d=this,e=this.$element.hasClass("fade")?"fade":"";if(this.isShown&&this.options.backdrop){var f=a.support.transition&&e;if(this.$backdrop=a(document.createElement("div")).addClass("modal-backdrop "+e).appendTo(this.$body),this.$element.on("click.dismiss.bs.modal",a.proxy(function(a){return this.ignoreBackdropClick?void(this.ignoreBackdropClick=!1):void(a.target===a.currentTarget&&("static"==this.options.backdrop?this.$element[0].focus():this.hide()))},this)),f&&this.$backdrop[0].offsetWidth,this.$backdrop.addClass("in"),!b)return;f?this.$backdrop.one("bsTransitionEnd",b).emulateTransitionEnd(c.BACKDROP_TRANSITION_DURATION):b()}else if(!this.isShown&&this.$backdrop){this.$backdrop.removeClass("in");var g=function(){d.removeBackdrop(),b&&b()};a.support.transition&&this.$element.hasClass("fade")?this.$backdrop.one("bsTransitionEnd",g).emulateTransitionEnd(c.BACKDROP_TRANSITION_DURATION):g()}else b&&b()},c.prototype.handleUpdate=function(){this.adjustDialog()},c.prototype.adjustDialog=function(){var a=this.$element[0].scrollHeight>document.documentElement.clientHeight;this.$element.css({paddingLeft:!this.bodyIsOverflowing&&a?this.scrollbarWidth:"",paddingRight:this.bodyIsOverflowing&&!a?this.scrollbarWidth:""})},c.prototype.resetAdjustments=function(){this.$element.css({paddingLeft:"",paddingRight:""})},c.prototype.checkScrollbar=function(){var a=window.innerWidth;if(!a){var b=document.documentElement.getBoundingClientRect();a=b.right-Math.abs(b.left)}this.bodyIsOverflowing=document.body.clientWidth<a,this.scrollbarWidth=this.measureScrollbar()},c.prototype.setScrollbar=function(){var a=parseInt(this.$body.css("padding-right")||0,10);this.originalBodyPad=document.body.style.paddingRight||"",this.bodyIsOverflowing&&this.$body.css("padding-right",a+this.scrollbarWidth)},c.prototype.resetScrollbar=function(){this.$body.css("padding-right",this.originalBodyPad)},c.prototype.measureScrollbar=function(){var a=document.createElement("div");a.className="modal-scrollbar-measure",this.$body.append(a);var b=a.offsetWidth-a.clientWidth;return this.$body[0].removeChild(a),b};var d=a.fn.modal;a.fn.modal=b,a.fn.modal.Constructor=c,a.fn.modal.noConflict=function(){return a.fn.modal=d,this},a(document).on("click.bs.modal.data-api",'[data-toggle="modal"]',function(c){var d=a(this),e=d.attr("href"),f=a(d.attr("data-target")||e&&e.replace(/.*(?=#[^\s]+$)/,"")),g=f.data("bs.modal")?"toggle":a.extend({remote:!/#/.test(e)&&e},f.data(),d.data());d.is("a")&&c.preventDefault(),f.one("show.bs.modal",function(a){a.isDefaultPrevented()||f.one("hidden.bs.modal",function(){d.is(":visible")&&d.trigger("focus")})}),b.call(f,g,this)})}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var d=a(this),e=d.data("bs.tooltip"),f="object"==typeof b&&b;(e||!/destroy|hide/.test(b))&&(e||d.data("bs.tooltip",e=new c(this,f)),"string"==typeof b&&e[b]())})}var c=function(a,b){this.type=null,this.options=null,this.enabled=null,this.timeout=null,this.hoverState=null,this.$element=null,this.inState=null,this.init("tooltip",a,b)};c.VERSION="3.3.6",c.TRANSITION_DURATION=150,c.DEFAULTS={animation:!0,placement:"top",selector:!1,template:'<div class="tooltip" role="tooltip"><div class="tooltip-arrow"></div><div class="tooltip-inner"></div></div>',trigger:"hover focus",title:"",delay:0,html:!1,container:!1,viewport:{selector:"body",padding:0}},c.prototype.init=function(b,c,d){if(this.enabled=!0,this.type=b,this.$element=a(c),this.options=this.getOptions(d),this.$viewport=this.options.viewport&&a(a.isFunction(this.options.viewport)?this.options.viewport.call(this,this.$element):this.options.viewport.selector||this.options.viewport),this.inState={click:!1,hover:!1,focus:!1},this.$element[0]instanceof document.constructor&&!this.options.selector)throw new Error("`selector` option must be specified when initializing "+this.type+" on the window.document object!");for(var e=this.options.trigger.split(" "),f=e.length;f--;){var g=e[f];if("click"==g)this.$element.on("click."+this.type,this.options.selector,a.proxy(this.toggle,this));else if("manual"!=g){var h="hover"==g?"mouseenter":"focusin",i="hover"==g?"mouseleave":"focusout";this.$element.on(h+"."+this.type,this.options.selector,a.proxy(this.enter,this)),this.$element.on(i+"."+this.type,this.options.selector,a.proxy(this.leave,this))}}this.options.selector?this._options=a.extend({},this.options,{trigger:"manual",selector:""}):this.fixTitle()},c.prototype.getDefaults=function(){return c.DEFAULTS},c.prototype.getOptions=function(b){return b=a.extend({},this.getDefaults(),this.$element.data(),b),b.delay&&"number"==typeof b.delay&&(b.delay={show:b.delay,hide:b.delay}),b},c.prototype.getDelegateOptions=function(){var b={},c=this.getDefaults();return this._options&&a.each(this._options,function(a,d){c[a]!=d&&(b[a]=d)}),b},c.prototype.enter=function(b){var c=b instanceof this.constructor?b:a(b.currentTarget).data("bs."+this.type);return c||(c=new this.constructor(b.currentTarget,this.getDelegateOptions()),a(b.currentTarget).data("bs."+this.type,c)),b instanceof a.Event&&(c.inState["focusin"==b.type?"focus":"hover"]=!0),c.tip().hasClass("in")||"in"==c.hoverState?void(c.hoverState="in"):(clearTimeout(c.timeout),c.hoverState="in",c.options.delay&&c.options.delay.show?void(c.timeout=setTimeout(function(){"in"==c.hoverState&&c.show()},c.options.delay.show)):c.show())},c.prototype.isInStateTrue=function(){for(var a in this.inState)if(this.inState[a])return!0;return!1},c.prototype.leave=function(b){var c=b instanceof this.constructor?b:a(b.currentTarget).data("bs."+this.type);return c||(c=new this.constructor(b.currentTarget,this.getDelegateOptions()),a(b.currentTarget).data("bs."+this.type,c)),b instanceof a.Event&&(c.inState["focusout"==b.type?"focus":"hover"]=!1),c.isInStateTrue()?void 0:(clearTimeout(c.timeout),c.hoverState="out",c.options.delay&&c.options.delay.hide?void(c.timeout=setTimeout(function(){"out"==c.hoverState&&c.hide()},c.options.delay.hide)):c.hide())},c.prototype.show=function(){var b=a.Event("show.bs."+this.type);if(this.hasContent()&&this.enabled){this.$element.trigger(b);var d=a.contains(this.$element[0].ownerDocument.documentElement,this.$element[0]);if(b.isDefaultPrevented()||!d)return;var e=this,f=this.tip(),g=this.getUID(this.type);this.setContent(),f.attr("id",g),this.$element.attr("aria-describedby",g),this.options.animation&&f.addClass("fade");var h="function"==typeof this.options.placement?this.options.placement.call(this,f[0],this.$element[0]):this.options.placement,i=/\s?auto?\s?/i,j=i.test(h);j&&(h=h.replace(i,"")||"top"),f.detach().css({top:0,left:0,display:"block"}).addClass(h).data("bs."+this.type,this),this.options.container?f.appendTo(this.options.container):f.insertAfter(this.$element),this.$element.trigger("inserted.bs."+this.type);var k=this.getPosition(),l=f[0].offsetWidth,m=f[0].offsetHeight;if(j){var n=h,o=this.getPosition(this.$viewport);h="bottom"==h&&k.bottom+m>o.bottom?"top":"top"==h&&k.top-m<o.top?"bottom":"right"==h&&k.right+l>o.width?"left":"left"==h&&k.left-l<o.left?"right":h,f.removeClass(n).addClass(h)}var p=this.getCalculatedOffset(h,k,l,m);this.applyPlacement(p,h);var q=function(){var a=e.hoverState;e.$element.trigger("shown.bs."+e.type),e.hoverState=null,"out"==a&&e.leave(e)};a.support.transition&&this.$tip.hasClass("fade")?f.one("bsTransitionEnd",q).emulateTransitionEnd(c.TRANSITION_DURATION):q()}},c.prototype.applyPlacement=function(b,c){var d=this.tip(),e=d[0].offsetWidth,f=d[0].offsetHeight,g=parseInt(d.css("margin-top"),10),h=parseInt(d.css("margin-left"),10);isNaN(g)&&(g=0),isNaN(h)&&(h=0),b.top+=g,b.left+=h,a.offset.setOffset(d[0],a.extend({using:function(a){d.css({top:Math.round(a.top),left:Math.round(a.left)})}},b),0),d.addClass("in");var i=d[0].offsetWidth,j=d[0].offsetHeight;"top"==c&&j!=f&&(b.top=b.top+f-j);var k=this.getViewportAdjustedDelta(c,b,i,j);k.left?b.left+=k.left:b.top+=k.top;var l=/top|bottom/.test(c),m=l?2*k.left-e+i:2*k.top-f+j,n=l?"offsetWidth":"offsetHeight";d.offset(b),this.replaceArrow(m,d[0][n],l)},c.prototype.replaceArrow=function(a,b,c){this.arrow().css(c?"left":"top",50*(1-a/b)+"%").css(c?"top":"left","")},c.prototype.setContent=function(){var a=this.tip(),b=this.getTitle();a.find(".tooltip-inner")[this.options.html?"html":"text"](b),a.removeClass("fade in top bottom left right")},c.prototype.hide=function(b){function d(){"in"!=e.hoverState&&f.detach(),e.$element.removeAttr("aria-describedby").trigger("hidden.bs."+e.type),b&&b()}var e=this,f=a(this.$tip),g=a.Event("hide.bs."+this.type);return this.$element.trigger(g),g.isDefaultPrevented()?void 0:(f.removeClass("in"),a.support.transition&&f.hasClass("fade")?f.one("bsTransitionEnd",d).emulateTransitionEnd(c.TRANSITION_DURATION):d(),this.hoverState=null,this)},c.prototype.fixTitle=function(){var a=this.$element;(a.attr("title")||"string"!=typeof a.attr("data-original-title"))&&a.attr("data-original-title",a.attr("title")||"").attr("title","")},c.prototype.hasContent=function(){return this.getTitle()},c.prototype.getPosition=function(b){b=b||this.$element;var c=b[0],d="BODY"==c.tagName,e=c.getBoundingClientRect();null==e.width&&(e=a.extend({},e,{width:e.right-e.left,height:e.bottom-e.top}));var f=d?{top:0,left:0}:b.offset(),g={scroll:d?document.documentElement.scrollTop||document.body.scrollTop:b.scrollTop()},h=d?{width:a(window).width(),height:a(window).height()}:null;return a.extend({},e,g,h,f)},c.prototype.getCalculatedOffset=function(a,b,c,d){return"bottom"==a?{top:b.top+b.height,left:b.left+b.width/2-c/2}:"top"==a?{top:b.top-d,left:b.left+b.width/2-c/2}:"left"==a?{top:b.top+b.height/2-d/2,left:b.left-c}:{top:b.top+b.height/2-d/2,left:b.left+b.width}},c.prototype.getViewportAdjustedDelta=function(a,b,c,d){var e={top:0,left:0};if(!this.$viewport)return e;var f=this.options.viewport&&this.options.viewport.padding||0,g=this.getPosition(this.$viewport);if(/right|left/.test(a)){var h=b.top-f-g.scroll,i=b.top+f-g.scroll+d;h<g.top?e.top=g.top-h:i>g.top+g.height&&(e.top=g.top+g.height-i)}else{var j=b.left-f,k=b.left+f+c;j<g.left?e.left=g.left-j:k>g.right&&(e.left=g.left+g.width-k)}return e},c.prototype.getTitle=function(){var a,b=this.$element,c=this.options;return a=b.attr("data-original-title")||("function"==typeof c.title?c.title.call(b[0]):c.title)},c.prototype.getUID=function(a){do a+=~~(1e6*Math.random());while(document.getElementById(a));return a},c.prototype.tip=function(){if(!this.$tip&&(this.$tip=a(this.options.template),1!=this.$tip.length))throw new Error(this.type+" `template` option must consist of exactly 1 top-level element!");return this.$tip},c.prototype.arrow=function(){return this.$arrow=this.$arrow||this.tip().find(".tooltip-arrow")},c.prototype.enable=function(){this.enabled=!0},c.prototype.disable=function(){this.enabled=!1},c.prototype.toggleEnabled=function(){this.enabled=!this.enabled},c.prototype.toggle=function(b){var c=this;b&&(c=a(b.currentTarget).data("bs."+this.type),c||(c=new this.constructor(b.currentTarget,this.getDelegateOptions()),a(b.currentTarget).data("bs."+this.type,c))),b?(c.inState.click=!c.inState.click,c.isInStateTrue()?c.enter(c):c.leave(c)):c.tip().hasClass("in")?c.leave(c):c.enter(c)},c.prototype.destroy=function(){var a=this;clearTimeout(this.timeout),this.hide(function(){a.$element.off("."+a.type).removeData("bs."+a.type),a.$tip&&a.$tip.detach(),a.$tip=null,a.$arrow=null,a.$viewport=null})};var d=a.fn.tooltip;a.fn.tooltip=b,a.fn.tooltip.Constructor=c,a.fn.tooltip.noConflict=function(){return a.fn.tooltip=d,this}}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var d=a(this),e=d.data("bs.popover"),f="object"==typeof b&&b;(e||!/destroy|hide/.test(b))&&(e||d.data("bs.popover",e=new c(this,f)),"string"==typeof b&&e[b]())})}var c=function(a,b){this.init("popover",a,b)};if(!a.fn.tooltip)throw new Error("Popover requires tooltip.js");c.VERSION="3.3.6",c.DEFAULTS=a.extend({},a.fn.tooltip.Constructor.DEFAULTS,{placement:"right",trigger:"click",content:"",template:'<div class="popover" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>'}),c.prototype=a.extend({},a.fn.tooltip.Constructor.prototype),c.prototype.constructor=c,c.prototype.getDefaults=function(){return c.DEFAULTS},c.prototype.setContent=function(){var a=this.tip(),b=this.getTitle(),c=this.getContent();a.find(".popover-title")[this.options.html?"html":"text"](b),a.find(".popover-content").children().detach().end()[this.options.html?"string"==typeof c?"html":"append":"text"](c),a.removeClass("fade top bottom left right in"),a.find(".popover-title").html()||a.find(".popover-title").hide()},c.prototype.hasContent=function(){return this.getTitle()||this.getContent()},c.prototype.getContent=function(){var a=this.$element,b=this.options;return a.attr("data-content")||("function"==typeof b.content?b.content.call(a[0]):b.content)},c.prototype.arrow=function(){return this.$arrow=this.$arrow||this.tip().find(".arrow")};var d=a.fn.popover;a.fn.popover=b,a.fn.popover.Constructor=c,a.fn.popover.noConflict=function(){return a.fn.popover=d,this}}(jQuery),+function(a){"use strict";function b(c,d){this.$body=a(document.body),this.$scrollElement=a(a(c).is(document.body)?window:c),this.options=a.extend({},b.DEFAULTS,d),this.selector=(this.options.target||"")+" .nav li > a",this.offsets=[],this.targets=[],this.activeTarget=null,this.scrollHeight=0,this.$scrollElement.on("scroll.bs.scrollspy",a.proxy(this.process,this)),this.refresh(),this.process()}function c(c){return this.each(function(){var d=a(this),e=d.data("bs.scrollspy"),f="object"==typeof c&&c;e||d.data("bs.scrollspy",e=new b(this,f)),"string"==typeof c&&e[c]()})}b.VERSION="3.3.6",b.DEFAULTS={offset:10},b.prototype.getScrollHeight=function(){return this.$scrollElement[0].scrollHeight||Math.max(this.$body[0].scrollHeight,document.documentElement.scrollHeight)},b.prototype.refresh=function(){var b=this,c="offset",d=0;this.offsets=[],this.targets=[],this.scrollHeight=this.getScrollHeight(),a.isWindow(this.$scrollElement[0])||(c="position",d=this.$scrollElement.scrollTop()),this.$body.find(this.selector).map(function(){var b=a(this),e=b.data("target")||b.attr("href"),f=/^#./.test(e)&&a(e);return f&&f.length&&f.is(":visible")&&[[f[c]().top+d,e]]||null}).sort(function(a,b){return a[0]-b[0]}).each(function(){b.offsets.push(this[0]),b.targets.push(this[1])})},b.prototype.process=function(){var a,b=this.$scrollElement.scrollTop()+this.options.offset,c=this.getScrollHeight(),d=this.options.offset+c-this.$scrollElement.height(),e=this.offsets,f=this.targets,g=this.activeTarget;if(this.scrollHeight!=c&&this.refresh(),b>=d)return g!=(a=f[f.length-1])&&this.activate(a);if(g&&b<e[0])return this.activeTarget=null,this.clear();for(a=e.length;a--;)g!=f[a]&&b>=e[a]&&(void 0===e[a+1]||b<e[a+1])&&this.activate(f[a])},b.prototype.activate=function(b){this.activeTarget=b,this.clear();var c=this.selector+'[data-target="'+b+'"],'+this.selector+'[href="'+b+'"]',d=a(c).parents("li").addClass("active");
d.parent(".dropdown-menu").length&&(d=d.closest("li.dropdown").addClass("active")),d.trigger("activate.bs.scrollspy")},b.prototype.clear=function(){a(this.selector).parentsUntil(this.options.target,".active").removeClass("active")};var d=a.fn.scrollspy;a.fn.scrollspy=c,a.fn.scrollspy.Constructor=b,a.fn.scrollspy.noConflict=function(){return a.fn.scrollspy=d,this},a(window).on("load.bs.scrollspy.data-api",function(){a('[data-spy="scroll"]').each(function(){var b=a(this);c.call(b,b.data())})})}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var d=a(this),e=d.data("bs.tab");e||d.data("bs.tab",e=new c(this)),"string"==typeof b&&e[b]()})}var c=function(b){this.element=a(b)};c.VERSION="3.3.6",c.TRANSITION_DURATION=150,c.prototype.show=function(){var b=this.element,c=b.closest("ul:not(.dropdown-menu)"),d=b.data("target");if(d||(d=b.attr("href"),d=d&&d.replace(/.*(?=#[^\s]*$)/,"")),!b.parent("li").hasClass("active")){var e=c.find(".active:last a"),f=a.Event("hide.bs.tab",{relatedTarget:b[0]}),g=a.Event("show.bs.tab",{relatedTarget:e[0]});if(e.trigger(f),b.trigger(g),!g.isDefaultPrevented()&&!f.isDefaultPrevented()){var h=a(d);this.activate(b.closest("li"),c),this.activate(h,h.parent(),function(){e.trigger({type:"hidden.bs.tab",relatedTarget:b[0]}),b.trigger({type:"shown.bs.tab",relatedTarget:e[0]})})}}},c.prototype.activate=function(b,d,e){function f(){g.removeClass("active").find("> .dropdown-menu > .active").removeClass("active").end().find('[data-toggle="tab"]').attr("aria-expanded",!1),b.addClass("active").find('[data-toggle="tab"]').attr("aria-expanded",!0),h?(b[0].offsetWidth,b.addClass("in")):b.removeClass("fade"),b.parent(".dropdown-menu").length&&b.closest("li.dropdown").addClass("active").end().find('[data-toggle="tab"]').attr("aria-expanded",!0),e&&e()}var g=d.find("> .active"),h=e&&a.support.transition&&(g.length&&g.hasClass("fade")||!!d.find("> .fade").length);g.length&&h?g.one("bsTransitionEnd",f).emulateTransitionEnd(c.TRANSITION_DURATION):f(),g.removeClass("in")};var d=a.fn.tab;a.fn.tab=b,a.fn.tab.Constructor=c,a.fn.tab.noConflict=function(){return a.fn.tab=d,this};var e=function(c){c.preventDefault(),b.call(a(this),"show")};a(document).on("click.bs.tab.data-api",'[data-toggle="tab"]',e).on("click.bs.tab.data-api",'[data-toggle="pill"]',e)}(jQuery),+function(a){"use strict";function b(b){return this.each(function(){var d=a(this),e=d.data("bs.affix"),f="object"==typeof b&&b;e||d.data("bs.affix",e=new c(this,f)),"string"==typeof b&&e[b]()})}var c=function(b,d){this.options=a.extend({},c.DEFAULTS,d),this.$target=a(this.options.target).on("scroll.bs.affix.data-api",a.proxy(this.checkPosition,this)).on("click.bs.affix.data-api",a.proxy(this.checkPositionWithEventLoop,this)),this.$element=a(b),this.affixed=null,this.unpin=null,this.pinnedOffset=null,this.checkPosition()};c.VERSION="3.3.6",c.RESET="affix affix-top affix-bottom",c.DEFAULTS={offset:0,target:window},c.prototype.getState=function(a,b,c,d){var e=this.$target.scrollTop(),f=this.$element.offset(),g=this.$target.height();if(null!=c&&"top"==this.affixed)return c>e?"top":!1;if("bottom"==this.affixed)return null!=c?e+this.unpin<=f.top?!1:"bottom":a-d>=e+g?!1:"bottom";var h=null==this.affixed,i=h?e:f.top,j=h?g:b;return null!=c&&c>=e?"top":null!=d&&i+j>=a-d?"bottom":!1},c.prototype.getPinnedOffset=function(){if(this.pinnedOffset)return this.pinnedOffset;this.$element.removeClass(c.RESET).addClass("affix");var a=this.$target.scrollTop(),b=this.$element.offset();return this.pinnedOffset=b.top-a},c.prototype.checkPositionWithEventLoop=function(){setTimeout(a.proxy(this.checkPosition,this),1)},c.prototype.checkPosition=function(){if(this.$element.is(":visible")){var b=this.$element.height(),d=this.options.offset,e=d.top,f=d.bottom,g=Math.max(a(document).height(),a(document.body).height());"object"!=typeof d&&(f=e=d),"function"==typeof e&&(e=d.top(this.$element)),"function"==typeof f&&(f=d.bottom(this.$element));var h=this.getState(g,b,e,f);if(this.affixed!=h){null!=this.unpin&&this.$element.css("top","");var i="affix"+(h?"-"+h:""),j=a.Event(i+".bs.affix");if(this.$element.trigger(j),j.isDefaultPrevented())return;this.affixed=h,this.unpin="bottom"==h?this.getPinnedOffset():null,this.$element.removeClass(c.RESET).addClass(i).trigger(i.replace("affix","affixed")+".bs.affix")}"bottom"==h&&this.$element.offset({top:g-b-f})}};var d=a.fn.affix;a.fn.affix=b,a.fn.affix.Constructor=c,a.fn.affix.noConflict=function(){return a.fn.affix=d,this},a(window).on("load",function(){a('[data-spy="affix"]').each(function(){var c=a(this),d=c.data();d.offset=d.offset||{},null!=d.offsetBottom&&(d.offset.bottom=d.offsetBottom),null!=d.offsetTop&&(d.offset.top=d.offsetTop),b.call(c,d)})})}(jQuery);
/* Generated by Opal 0.8.1 */
Opal.modules["systems_view"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$find', '$setup', '$attr_reader', '$say_hello_when_a_link_is_clicked', '$on', '$puts', '$text', '$current_target', '$all_links', '$private', '$element']);
  return (function($base, $super) {
    function $SystemsView(){};
    var self = $SystemsView = $klass($base, $super, 'SystemsView', $SystemsView);

    var def = self.$$proto, $scope = self.$$scope;

    def.all_links = nil;
    def.$initialize = function(selector, parent) {
      var self = this;

      if (selector == null) {
        selector = "body.controller-systems"
      }
      if (parent == null) {
        parent = $scope.get('Element')
      }
      self.element = parent.$find(selector);
      return self.$setup();
    };

    self.$attr_reader("element");

    def.$setup = function() {
      var self = this;

      return self.$say_hello_when_a_link_is_clicked();
    };

    def.$say_hello_when_a_link_is_clicked = function() {
      var $a, $b, TMP_1, self = this;

      return ($a = ($b = self.$all_links()).$on, $a.$$p = (TMP_1 = function(event){var self = TMP_1.$$s || this;
if (event == null) event = nil;
      return self.$puts("Hello! (You just clicked on a link: " + (event.$current_target().$text()) + ")")}, TMP_1.$$s = self, TMP_1), $a).call($b, "click");
    };

    self.$private();

    return (def.$all_links = function() {
      var $a, self = this;

      return ((($a = self.all_links) !== false && $a !== nil) ? $a : self.all_links = self.$element().$find("a"));
    }, nil) && 'all_links';
  })(self, null)
};
/* Generated by Opal 0.8.1 */
Opal.modules["tops_view"] = function(Opal) {
  Opal.dynamic_require_severity = "ignore";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$find', '$setup', '$attr_reader', '$say_hello_when_a_link_is_clicked', '$on', '$puts', '$text', '$current_target', '$all_links', '$private', '$element']);
  return (function($base, $super) {
    function $TopsView(){};
    var self = $TopsView = $klass($base, $super, 'TopsView', $TopsView);

    var def = self.$$proto, $scope = self.$$scope;

    def.all_links = nil;
    def.$initialize = function(selector, parent) {
      var self = this;

      if (selector == null) {
        selector = "body.controller-tops"
      }
      if (parent == null) {
        parent = $scope.get('Element')
      }
      self.element = parent.$find(selector);
      return self.$setup();
    };

    self.$attr_reader("element");

    def.$setup = function() {
      var self = this;

      return self.$say_hello_when_a_link_is_clicked();
    };

    def.$say_hello_when_a_link_is_clicked = function() {
      var $a, $b, TMP_1, self = this;

      return ($a = ($b = self.$all_links()).$on, $a.$$p = (TMP_1 = function(event){var self = TMP_1.$$s || this;
if (event == null) event = nil;
      return self.$puts("Hello! (You just clicked on a link: " + (event.$current_target().$text()) + ")")}, TMP_1.$$s = self, TMP_1), $a).call($b, "click");
    };

    self.$private();

    return (def.$all_links = function() {
      var $a, self = this;

      return ((($a = self.all_links) !== false && $a !== nil) ? $a : self.all_links = self.$element().$find("a"));
    }, nil) && 'all_links';
  })(self, null)
};
(function() {


}).call(this);
(function() {


}).call(this);
