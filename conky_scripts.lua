function conky_format_cpu(number)
   return string.format('%2i', conky_parse(number))
end

function conky_format_bat(number)
   return string.format('%3i', conky_parse(number))
end
