json.array!(@works) do |work|
  json.extract! work, :id, :name
  json.url work_url(work, format: :json)
end
