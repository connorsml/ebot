$:.unshift(File.expand_path("./lib", ENV["rvm_path"]))
require "rvm/capistrano"

default_run_options[:pty] = true

set :application, "mo_crawler"
set :scm, :mercurial
set :repository,  "https://hg.webestudio.ru/erlang/mo_crawler/"
set :branch, "default"
set :scm_username, "akopylov"
set :scm_password, "6WeIdHf"
set :user, "user"
set :deploy_to, "/opt/apps/mo_crawler/"
set :shared_host, "ads.motmom.com"
set :keep_releases, 2
set :use_sudo, false

set :rvm_ruby_string, "1.9.2"
set :rvm_type, :user

role :web, shared_host
role :app, shared_host
role :db,  shared_host, :primary => true

after "deploy:update_code", "deploy:config"
after "deploy:update_code", "deploy:setup_log"
after "deploy:update", "deploy:cleanup"

set :whenever_command, "bundle exec whenever"
require "whenever/capistrano"

namespace :deploy do
  task :start do
		deploy.stop_crawler
		deploy.start_crawler
  end

  task :stop do
  	deploy.stop_crawler
  end

  task :restart do
  	deploy.stop_crawler
		deploy.start_crawler  	
  end

  task :stop_crawler do
		run "cd #{deploy_to}/current/script && ./kill_crawler"
  end

  task :start_crawler do
		run "cd #{deploy_to}/current/script && ./reanimate"
  end

  task :config do
    run "cd #{release_path} && ln -s #{shared_path}/config/sys.config ."
    run "cd #{release_path} && make clean && make"
    # run "cd #{release_path} && /usr/bin/env bundle update RAILS_ENV=#{rails_env}"
  end

  task :setup_log do
    run "cd #{release_path} && rm -rf ./priv/log && ln -s #{shared_path}/log ./priv/log"
  end


end