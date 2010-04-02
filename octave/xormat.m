function r = xorify(A)
	r = mod(A,2);
end

function r = mul(A,B)
	r = xorify(A * B);
end

function r = add(A,B)
	r = xorify(A + B);
end

function P = getP(it,jt,v)
	P = eye(length(v));
	for ix = 1 : length(v)
		if(ix != jt)
			P(:,ix) = add(P(:,ix),mul(v(ix),P(:,jt)));
		end
	end
	t = P(:,it);
	P(:,it) = P(:,jt);
	P(:,jt) = t;
end

function [Ir,Ar,Fr] = Simi(it,jt,v,I,A,F)
	P = getP(it,jt,v);
	Pi = xorify(inv(P));
	for ix = 1:length(A(1,1,:))
		Ar(:,:,ix) = mul(mul(Pi,A(:,:,ix)),P);
	end
	Ir = mul(I,P);
	Fr = mul(Pi,F);
end

function ix = lastzero(v)
	for ix = length(v):-1:1
		if(v(ix))
			return
		end
	end
end

function [I,A,F,d] = lacc(I,A,F)
	if(sum(I) == 0)
		I=[]
		A = []
		F=[]
		return
	end
	a = 1; b = 1;
	jt = lastzero(I);
	[I,A,F] = Simi(b,jt,I,I,A,F); 
	b++;
	while (a < b)
		q = a++;
	    for ix = 1:length(A(1,1,:))
			v = A(q,:,ix);
			jt = lastzero(v);
			if(jt >= b)
				[I,A,F] = Simi(b,jt,v,I,A,F);b++;
			end
		end
	end
	d = b-1;
	I = I(1:d);
	F = F(1:d);
	A = A(1:d,1:d,:);
end

function [I,A,F,d] = minNXA(I,A,F)
	It = F';
	Ft = I';
    for ix = 1:length(A(1,1,:))
		At(:,:,ix) = A(:,:,ix)';
	end
	[I,A,F,d] = lacc(It,At,Ft)
	It = F';
	Ft = I';
	for ix = 1:length(A(1,1,:))
		At(:,:,ix) = A(:,:,ix)';
	end
	[I,A,F,d] = lacc(It,At,Ft)
end





