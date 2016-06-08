#!/home/kppn/.rbenv/shims/ruby

require 'fileutils'


def change_worker_num(unicorn_master_pid, sig_name, n)
  n.times do
    Process.kill(sig_name, unicorn_master_pid)
    sleep 0.1   # 一気にシグナルを送るとunicornが受け付けない
  end
end

def incl_workers(unicorn_master_pid, n)
  change_worker_num(unicorn_master_pid, :TTIN, n)
end

def decl_workers(unicorn_master_pid, n)
  change_worker_num(unicorn_master_pid, :TTOU, n)
end


def get_unicorn_pids
  unicorns = `ps aux`.each_line
                     .grep_v(/grep/)
                     .grep(/unicorn_rails/)
  unicorn_master_pid = unicorns.grep(/master/).first.split[1].to_i
  unicorn_worker_pids = unicorns.grep(/worker/).map{|v| v.split[1].to_i}

  [unicorn_master_pid, unicorn_worker_pids]
end


def usage
  puts <<~EOL
         set_console.rb
           RailsのWEBコンソールをOn/Offする
           現状だとunicorn_railsを利用した場合にしか対応していない。

           usage
             * 準備
                 consoleを呼び出す箇所に以下のコードを記載する
                     console if ENV['rails_web_console']
             * On
                 set_console.rb on
             * Off
                 set_console.rb off
       EOL
end


unless ARGV.length > 0
  usage
  exit
end
action = ARGV[0]

unicorn_master_pid, unicorn_worker_pids = get_unicorn_pids

case action
when 'on'
  # masterに ENV['rails_web_console'] をセット
  File.open('gdb_command', 'w') do |f|
    f.puts ['call putenv("rails_web_console=true")', 'detach', 'quit'].join("\n")
  end
  `gdb -p #{unicorn_master_pid} -x gdb_command`

  # worker に 環境変数を反映させるため上げ直す
  decl_workers(unicorn_master_pid, unicorn_worker_pids.length)
  incl_workers(unicorn_master_pid, 1)
when 'off'
  # masterから ENV['rails_web_console'] をアンセット
  File.open('gdb_command', 'w') do |f|
    f.puts ['call unsetenv("rails_web_console")', 'detach', 'quit'].join("\n")
  end
  `gdb -p #{unicorn_master_pid} -x gdb_command`

  # worker に 環境変数を反映させるため上げ直す (masterを再起動してworkerを元に戻す)
  decl_workers(unicorn_master_pid, unicorn_worker_pids.length)
  Process.kill(:USR2, unicorn_master_pid)
end
`rm gdb_command`


