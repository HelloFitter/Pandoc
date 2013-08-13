function caps(key, val, format)
  if key == 'Str' then
    return 'Str', string.upper(val)
  end
end

transform = filter(caps)
