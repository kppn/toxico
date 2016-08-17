class CreateCodes < ActiveRecord::Migration
  def change
    create_table :codes do |t|
      t.belongs_to :work

      t.string :file

      t.timestamps null: false
    end
  end
end
