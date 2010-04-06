function C = cam_a(A,F,k)
	ix = 1;
	C(:,ix++) = F;
	k--;
	while(k > 0)
		[T xs] = revmultimes(A,ix-2);
		c =mul(T,F);
		C(:,ix++) = c;k--;
		xs
	end
end
