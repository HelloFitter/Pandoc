local function caps(k,v,f)
  if k == 'Str' then
    return 'Str', string.upper(v)
  end
end

local function deemph(k,v,f)
  if k == 'Emph' and f == 'html' then
    walk(v,caps,f)
    return 'Span', {{"",{},{}}, v}
  end
end

transform = filter(deemph)
