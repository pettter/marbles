function T = tmatrix(I,A,F,k)
	R = ram_a(I,A,k);
	C = cam_a(A,F,k);
	T = mul(R,C);
end
