function [S rn] = stroflen(r,n,k)
	s = zeros(1,n);
	ix = 1;
	S{ix++} = s;
	while(ix <= k)
		c = 1;
		for in = 1:n
			if(s(in) < r-1 )
				s(in)++;
				c = 0;
				break;
			else
				s(in) = 0;
			end
		end
		if(c == 1)
			break;
		end
		S{ix++} = s;
	end
	rn = ix-1;
end
