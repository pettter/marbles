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
