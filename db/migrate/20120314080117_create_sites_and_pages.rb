class CreateSitesAndPages < ActiveRecord::Migration
  
  def up

  	create_table :site_passes do |t|
  		t.string  :url, :null => false

  		t.integer :pass_number
  		t.string :crawling_finished_at
  		t.string :crawling_started_at
  		  		  		
  		t.string :ebot_created_at
  		t.string  :ebot_doctype

  		t.integer :new_links_count
  		t.integer :fetched_links_count
  		t.integer :processed_links_count
  		t.integer :refused_links_count
  		  		
  		t.text    :robots_txt
  	end
    
    add_index :site_passes, :url, :unique => true

  	create_table :site_pages do |t|
  		t.string  :url, :null => false  		
  		t.string :http_returncode
  		t.string :header_content_length
  		t.string  :ebot_created_at
  		t.integer :ebot_errors_count
  		t.string  :ebot_head_visited
  		t.string  :ebot_doctype
  		t.string  :ebot_body_visited
  		t.string  :header_server
  		t.string  :header_content_type
  		t.string  :title
  		t.string  :ebot_domain
  		t.integer :ebot_links_count
  		t.string  :headermeta
  		t.string  :ebot_referrals
  		t.integer :ebot_visits_count
  		t.string  :content_type
  		t.string  :header_x_powered_by
  		t.string  :digest
  		t.string  :ebot_head_error
  	end

    add_index :site_pages, :url, :unique => true
    add_index :site_pages, :digest

  end

  def down
    remove_index :site_passes, :url
    remove_index :site_pages, :url
    drop_table :site_passes

    remove_index :site_pages, :digest
  	drop_table :site_pages
  end

end
