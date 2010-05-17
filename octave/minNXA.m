function [I,A,F,d] = minNXA(I,A,F)
	It = F';
	Ft = I';
    for ix = 1:length(A(1,1,:))
		At(:,:,ix) = A(:,:,ix)';
	end
	[I,A,F,d] = lacc(It,At,Ft)
	It = F';
	Ft = I';
	At = [];
	for ix = 1:length(A(1,1,:))
		At(:,:,ix) = A(:,:,ix)';
	end
	[I,A,F,d] = lacc(It,At,Ft)
end





