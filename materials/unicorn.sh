#!/bin/sh

bundle exec unicorn_rails  -c config/unicorn.rb -E development -D



# unicorn actions by signals
# 
# # master
# 
# INT / TERM      即死。gracefullじゃない
# QUIT            gracefullに死ぬ
# USR1            master,workerが開いてるログファイルを再openする
# USR2            masterごと再executeする。小シグナルがforkされたあとにはQUITシグナルを個別に送らないと元のプロセスは死なない
# WINCH           masterは残るけどworkerはgracefullに死ぬ。
# TTIN            workerの数を増やす
# TTOU            workerの数を減らす
# 
# # worker
# INT / TERM      workerを即死。masterが生きてる場合は、masterがrespawnする。
# QUIT            gracefullに死ぬ。masterが生きてる場合は、masterがrespawnする。
# USR1            workerが開いてるログファイルを再openする
 
