%w(
  app
).each { |path| Spring.watch(path) }

