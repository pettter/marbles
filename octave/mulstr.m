function R = mulstr(A,S)
	R = A(:,:,S(1)+1);
	for ix = 2:length(S)
		R = mul(R,A(:,:,S(ix)+1));
	end
end
