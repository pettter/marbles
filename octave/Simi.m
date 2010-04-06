function [Ir,Ar,Fr] = Simi(it,jt,v,I,A,F)
	P = getP(it,jt,v);
	Pi = xorify(inv(P));
	for ix = 1:length(A(1,1,:))
		Ar(:,:,ix) = mul(mul(Pi,A(:,:,ix)),P);
	end
	Ir = mul(I,P);
	Fr = mul(Pi,F);
end

