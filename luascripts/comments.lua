incomment = false

function comment(k,v)
  if k == 'RawBlock' then
    local f,s = unpack(v)
    local format = f['unFormat']
    if format == "html" then
      if string.match(s, "!-- BEGIN COMMENT --") then
        incomment = true
        return emptyArray()
      elseif string.match(s, "!-- END COMMENT --") then
        incomment = false
        return emptyArray()
      end
    end
  end
  if incomment then
    return emptyArray() -- suppress
  end
end

transform = filter(comment)
