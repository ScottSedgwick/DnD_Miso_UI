require 'rake/clean'

CLOBBER.include('dist-newstyle')
CLEAN.include('public')

def mysys(cmd)
  puts "Executing: [#{cmd}]"
  system cmd
end

namespace :js do
  task :update do
	  mysys 'cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg'
  end

  task :build do
	  mysys 'cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg'
	  mysys 'cp -v ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .'
	  mysys 'rm -rf public'
	  mysys 'cp -rv static public'
	  mysys 'bunx --bun swc ./all.js -o public/index.js'
  end

  task :all => ['js:update', 'js:build']
end

namespace :wasm do
  task :update do
    mysys 'wasm32-wasi-cabal update'
  end

  task :compile => [:clean] do
    mysys 'wasm32-wasi-cabal build '
  end

  task :build => ['wasm:compile'] do
    mysys 'mkdir -p public'
	  mysys 'cp -R static/* public'
    postlink = "#{`wasm32-wasi-ghc --print-libdir`.strip}/post-link.mjs"
    mywasm = `wasm32-wasi-cabal list-bin app | tail -n 1`.strip
	  mysys "#{postlink} --input #{mywasm} --output public/ghc_wasm_jsffi.js"
	  mysys "cp -v #{mywasm} public/"
  end

  task :optim do
	  mysys 'wasm-opt -all -O2 public/app.wasm -o public/app.wasm'
	  mysys 'wasm-tools strip -o public/app.wasm public/app.wasm'
  end

  task :all => ['wasm:update', 'wasm:build', 'wasm:optim']
end

task :build => ['wasm:build']
task :default => [:build]

task :repl => ['wasm:update'] do
  mysys 'wasm32-wasi-cabal repl app -finteractive --repl-options=\'-fghci-browser -fghci-browser-port=8080\''
end

task :watch do
  mysys 'ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch app/*.hs --debounce 50ms --command \'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"\''
end

task :serve do
	mysys 'http-server public'
end