{:repl {:plugins [[cider/cider-nrepl "0.15.1"]]
        :jvm-opts ["-Xms128m" "-Xmx256m"]}
 :user {:plugins [
                  [leipzig/lein-template "0.4.0-SNAPSHOT"]
                  [org.clojars.gmoe/overtone-template "1.0.1"]
                  ]}
 ;; unfortunatelly this is not working
 ;; see more on: https://github.com/gmoe/overtone-template/issues/2
 }
