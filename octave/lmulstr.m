function C = lmulstr(A,S)
	C = A(:,:,S(length(S))+1);
	for ix = length(S)-1:-1:1
		C = mul(A(:,:,S(ix)+1),C);
	end
end
