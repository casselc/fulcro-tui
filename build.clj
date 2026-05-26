(ns build
  "tools.build script for fulcro-tui.

   Tasks (run with `clojure -T:build <task>`):
     clean    delete the target directory
     jar      write the pom and build target/fulcro-tui-<version>.jar
     install  build the jar and install it into the local ~/.m2 repository
     deploy   build the jar and deploy it to Clojars

   Deploy credentials: deps-deploy reads the CLOJARS_USERNAME / CLOJARS_PASSWORD
   environment variables if set, otherwise it falls back to the <server> with
   id `clojars` in ~/.m2/settings.xml."
  (:require
   [clojure.tools.build.api :as b]
   [deps-deploy.deps-deploy :as dd]))

(def lib 'com.fulcrologic/fulcro-tui)
(def version "1.0.0-alpha1")
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar
  "Write the pom and build the library jar from src/main only."
  [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     @basis
                :src-dirs  ["src/main"]
                :scm       {:url                 "https://github.com/fulcrologic/fulcro-tui"
                            :connection          "scm:git:git://github.com/fulcrologic/fulcro-tui.git"
                            :developerConnection "scm:git:ssh://git@github.com/fulcrologic/fulcro-tui.git"
                            :tag                 (str "v" version)}
                :pom-data  [[:licenses
                             [:license
                              [:name "MIT License"]
                              [:url "https://opensource.org/license/mit"]]]]})
  (b/copy-dir {:src-dirs   ["src/main"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn install
  "Build the jar and install it into the local ~/.m2 repository."
  [_]
  (jar nil)
  (b/install {:basis     @basis
              :lib       lib
              :version   version
              :jar-file  jar-file
              :class-dir class-dir}))

(defn deploy
  "Build the jar and deploy it to Clojars."
  [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact  (b/resolve-path jar-file)
              :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}))
