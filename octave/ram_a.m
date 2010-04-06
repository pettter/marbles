function R = ram_a(I,A,k)
	ix = 1;
	n = 0;
	r = length(A(1,1,:));
	R(ix++,:) = I;
	k--;
	while(k > 0)
		n++;
		[S tk] = stroflen(r,n,k);
		for in = 1:length(S)
			R(ix++,:) = mul(I,mulstr(A,S{in}));
		end
		k = k - tk;
	end
end


