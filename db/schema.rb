# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20120322112529) do

  create_table "ad_categories", :force => true do |t|
    t.integer "ad_id"
    t.integer "category_id"
  end

  add_index "ad_categories", ["ad_id", "category_id"], :name => "index_ad_categories_on_ad_id_and_category_id", :unique => true

  create_table "ad_clicks", :force => true do |t|
    t.integer  "ad_platform_id"
    t.integer  "ad_id"
    t.text     "meta"
    t.datetime "created_at"
    t.decimal  "amount",         :precision => 10, :scale => 4
    t.boolean  "skip",                                          :default => true
    t.string   "user_guid"
  end

  add_index "ad_clicks", ["user_guid"], :name => "index_ad_clicks_on_user_guid"

  create_table "ad_companies", :force => true do |t|
    t.integer  "user_id"
    t.string   "name"
    t.string   "workflow_state"
    t.text     "options"
    t.decimal  "balance",        :precision => 10, :scale => 4, :default => 0.0
    t.datetime "created_at"
    t.datetime "updated_at"
    t.datetime "deleted_at"
  end

  create_table "ad_data", :force => true do |t|
    t.integer "datable_id",                                                   :null => false
    t.string  "datable_type",                                                 :null => false
    t.date    "store_date"
    t.integer "views_count",                                 :default => 0
    t.integer "clicks_count",                                :default => 0
    t.decimal "amount",       :precision => 10, :scale => 4, :default => 0.0
  end

  create_table "ad_locations", :force => true do |t|
    t.integer "ad_id"
    t.integer "city_location_id"
  end

  create_table "ad_platforms", :force => true do |t|
    t.string   "url"
    t.integer  "user_id"
    t.boolean  "approved",                                      :default => false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "workflow_state"
    t.decimal  "balance",        :precision => 10, :scale => 4, :default => 0.0
    t.decimal  "margin",         :precision => 10, :scale => 4
    t.datetime "deleted_at"
  end

  create_table "ad_regions", :force => true do |t|
    t.integer "ad_id"
    t.integer "region_id"
  end

  add_index "ad_regions", ["ad_id", "region_id"], :name => "index_ad_regions_on_ad_id_and_region_id", :unique => true

  create_table "ad_sites", :force => true do |t|
    t.integer "ad_id"
    t.integer "site_id"
  end

  add_index "ad_sites", ["ad_id", "site_id"], :name => "index_ad_sites_on_ad_id_and_site_id", :unique => true

  create_table "ad_views", :force => true do |t|
    t.integer  "ad_platform_id"
    t.integer  "ad_id"
    t.datetime "created_at"
  end

  create_table "ads", :force => true do |t|
    t.string   "content"
    t.string   "url"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.boolean  "active",                                                     :default => false
    t.integer  "views_count",                                                :default => 0
    t.integer  "clicks_count",                                               :default => 0
    t.string   "guid",          :limit => 36
    t.string   "title"
    t.integer  "ad_company_id"
    t.decimal  "price",                       :precision => 10, :scale => 4
    t.datetime "deleted_at"
  end

  add_index "ads", ["guid"], :name => "index_ads_on_guid"

  create_table "api_requests", :force => true do |t|
    t.string   "request_method"
    t.string   "request_path"
    t.string   "request_format"
    t.string   "remote_ip"
    t.string   "query_string"
    t.text     "request_body"
    t.integer  "response_code"
    t.text     "response_body"
    t.datetime "created_at",     :null => false
    t.datetime "updated_at",     :null => false
  end

  create_table "categories", :force => true do |t|
    t.string   "name_ru"
    t.string   "name_en"
    t.string   "slug"
    t.integer  "parent_id"
    t.integer  "lft"
    t.integer  "rgt"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "cat_url"
  end

  create_table "delayed_jobs", :force => true do |t|
    t.integer  "priority",   :default => 0
    t.integer  "attempts",   :default => 0
    t.text     "handler"
    t.text     "last_error"
    t.datetime "run_at"
    t.datetime "locked_at"
    t.datetime "failed_at"
    t.string   "locked_by"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "delayed_jobs", ["priority", "run_at"], :name => "delayed_jobs_priority"

  create_table "event_logs", :force => true do |t|
    t.integer  "user_id"
    t.string   "event"
    t.text     "data"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "ip_locations", :force => true do |t|
    t.integer  "pool_start",      :limit => 8
    t.integer  "pool_end",        :limit => 8
    t.string   "ip_address_pool"
    t.string   "country"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "region_id"
    t.integer  "update_count",                 :default => 0
  end

  add_index "ip_locations", ["pool_start", "pool_start"], :name => "index_ip_locations_on_pool_start_and_pool_start"

  create_table "pages", :force => true do |t|
    t.string  "url",                   :null => false
    t.string  "http_returncode"
    t.string  "header_content_length"
    t.string  "ebot_created_at"
    t.integer "ebot_errors_count"
    t.string  "ebot_head_visited"
    t.string  "ebot_doctype"
    t.string  "ebot_body_visited"
    t.string  "header_server"
    t.string  "header_content_type"
    t.string  "title"
    t.string  "ebot_domain"
    t.integer "ebot_links_count"
    t.string  "headermeta"
    t.string  "ebot_referrals"
    t.integer "ebot_visits_count"
    t.string  "content_type"
    t.string  "header_x_powered_by"
    t.string  "digest"
    t.string  "ebot_head_error"
  end

  add_index "pages", ["digest"], :name => "index_site_pages_on_digest"
  add_index "pages", ["url"], :name => "index_site_pages_on_url", :unique => true

  create_table "payments", :force => true do |t|
    t.integer  "user_id",                                                          :null => false
    t.decimal  "amount",           :precision => 10, :scale => 4, :default => 0.0
    t.datetime "created_at"
    t.integer  "paymentable_id",                                                   :null => false
    t.string   "paymentable_type",                                                 :null => false
  end

  create_table "regions", :force => true do |t|
    t.string   "name_ru"
    t.string   "name_en"
    t.string   "slug"
    t.string   "reg_url"
    t.integer  "parent_id"
    t.integer  "lft"
    t.integer  "rgt"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "loc_id"
    t.string   "loc_string"
    t.boolean  "show_in_cat"
    t.string   "alias"
  end

  add_index "regions", ["loc_id", "name_ru", "name_en"], :name => "index_regions_on_loc_id_and_name_ru_and_name_en"

  create_table "site_categories", :force => true do |t|
    t.integer "site_id"
    t.integer "category_id"
  end

  create_table "site_passes", :force => true do |t|
    t.string  "url",                   :null => false
    t.integer "pass_number"
    t.string  "crawling_finished_at"
    t.string  "crawling_started_at"
    t.string  "ebot_created_at"
    t.string  "ebot_doctype"
    t.integer "new_links_count"
    t.integer "fetched_links_count"
    t.integer "processed_links_count"
    t.integer "refused_links_count"
    t.text    "robots_txt"
  end

  add_index "site_passes", ["url"], :name => "index_site_passes_on_url", :unique => true

  create_table "site_regions", :force => true do |t|
    t.integer "site_id"
    t.integer "region_id"
  end

  create_table "sites", :force => true do |t|
    t.string   "url",              :limit => 1000,                    :null => false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.text     "description"
    t.boolean  "indexed",                          :default => false
    t.integer  "pages_count",                      :default => 0
    t.integer  "pass_count",                       :default => 0
    t.datetime "started_at"
    t.datetime "finished_at"
    t.string   "workflow_state"
    t.boolean  "approved",                         :default => false
    t.integer  "user_id"
    t.integer  "links_limit"
    t.string   "content_selector"
    t.integer  "views_count",                      :default => 0
    t.integer  "status"
    t.datetime "edited_at"
  end

  add_index "sites", ["url"], :name => "index_sites_on_url"

  create_table "taggings", :force => true do |t|
    t.integer  "tag_id"
    t.integer  "taggable_id"
    t.string   "taggable_type"
    t.integer  "tagger_id"
    t.string   "tagger_type"
    t.string   "context"
    t.datetime "created_at"
  end

  add_index "taggings", ["tag_id"], :name => "index_taggings_on_tag_id"
  add_index "taggings", ["taggable_id", "taggable_type", "context"], :name => "index_taggings_on_taggable_id_and_taggable_type_and_context"

  create_table "tags", :force => true do |t|
    t.string "name"
  end

  add_index "tags", ["name"], :name => "index_tags_on_name"

  create_table "users", :force => true do |t|
    t.string   "email",                                 :default => "", :null => false
    t.string   "encrypted_password",     :limit => 128, :default => "", :null => false
    t.string   "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.integer  "sign_in_count",                         :default => 0
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.string   "current_sign_in_ip"
    t.string   "last_sign_in_ip"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "roles"
    t.datetime "deleted_at"
  end

  add_index "users", ["email"], :name => "index_users_on_email", :unique => true
  add_index "users", ["reset_password_token"], :name => "index_users_on_reset_password_token", :unique => true

end
