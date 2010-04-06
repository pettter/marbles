function R = ram_a(I,A,k)
	ix = 1;
	R(ix++,:) = I;
	k--;
	while(k > 0)
		[T xs] = multimes(A,ix-2);
		r =mul(I,T);
		R(ix++,:) = r;k--;
		xs
	end
end


