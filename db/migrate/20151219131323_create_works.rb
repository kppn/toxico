class CreateWorks < ActiveRecord::Migration
  def change
    create_table :works do |t|
      t.string :name
      t.string :language
      t.string :file

      t.timestamps null: false
    end
  end
end
