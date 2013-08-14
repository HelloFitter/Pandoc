local function caps(k,v,f)
  if k == 'Str' then
    return 'Str', string.upper(v)
  end
end

local function deemph(k,v,f)
  if k == 'Emph' and f == 'html' then
    return walk(v,caps,f)
  end
end

transform = filter(deemph)
