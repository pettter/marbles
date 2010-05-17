function [I,A,F] = nlastsym(n)
	I = zeros(1,2**n);
	I(1) = 1;
	F = zeros(2**n,1);
	F((2**(n-1)+1):2**n) = 1;
	A = zeros(2**n,2**n,2);
	for ix = 1:(2**(n-1))
		A(ix,2*ix - 1,1) = 1;
		A(ix,2*ix,2) = 1;
	end
	A(2**(n-1)+1:2**n,:,:) = A(1:2**(n-1),:,:);
end
