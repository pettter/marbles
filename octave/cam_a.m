function C = cam_a(A,F,k)
	ix = 1;
	n = 0;
	r = length(A(1,1,:));
	C(:,ix++) = F;
	k--;
	while(k > 0)
		n++;
		[S tk] = stroflen(r,n,k);
		for in = 1:length(S)
			C(:,ix++) = mul(lmulstr(A,S{in}),F);
		end
		k = k - tk;
	end
end
