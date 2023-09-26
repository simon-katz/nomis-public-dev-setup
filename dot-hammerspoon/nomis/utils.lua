function nomisIndexof(array, value)
    for i, v in ipairs(array) do
        if v == value then
            return i
        end
    end
    return nil
end

function nomisContains(list, x)
   for _, v in pairs(list) do
      if v == x then return true end
   end
   return false
end
