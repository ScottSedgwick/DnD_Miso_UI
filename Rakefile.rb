require 'rake/clean'

CLOBBER.include('dist-newstyle')
CLEAN.include('public')

namespace :js do
  task :update do
	  system 'cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg'
  end

  task :build do
	  system 'cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg'
	  system 'cp -v ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .'
	  system 'rm -rf public'
	  system 'cp -rv static public'
	  system 'bunx --bun swc ./all.js -o public/index.js'
  end

  task :all => ['js:update', 'js:build']
end

namespace :wasm do
  task :update do
    system 'wasm32-wasi-cabal update'
  end

  task :compile => [:clean] do
    system 'wasm32-wasi-cabal build '
  end

  task :build => ['wasm:compile'] do
	  system 'cp -r static public'
    postlink = "#{`wasm32-wasi-ghc --print-libdir`.strip}/post-link.mjs"
    mywasm = `wasm32-wasi-cabal list-bin app | tail -n 1`.strip
	  system "#{postlink} --input #{mywasm} --output public/ghc_wasm_jsffi.js"
	  system "cp -v #{mywasm} public/"
  end

  task :optim do
	  system 'wasm-opt -all -O2 public/app.wasm -o public/app.wasm'
	  system 'wasm-tools strip -o public/app.wasm public/app.wasm'
  end

  task :all => ['wasm:update', 'wasm:build', 'wasm:optim']
end

task :build => ['wasm:build']
task :default => [:build]

task :repl => ['wasm:update'] do
  system 'wasm32-wasi-cabal repl app -finteractive --repl-options=\'-fghci-browser -fghci-browser-port=8080\''
end

task :watch do
  system 'ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch app/*.hs --debounce 50ms --command \'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"\''
end

task :serve do
	system 'http-server public'
end