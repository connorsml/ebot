class RenameSitePages < ActiveRecord::Migration
  
  def up
  	rename_table :site_pages, :pages
  end

  def down
  	rename_table :pages, :site_pages
  end

end
