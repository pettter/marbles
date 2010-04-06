function ix = lastzero(v)
	for ix = length(v):-1:1
		if(v(ix))
			return
		end
	end
end
