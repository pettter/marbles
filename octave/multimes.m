function [r,xs] = multimes(A,k)
	s = length(A(1,1,:));
	n = floor(k/s);
	if(n > 0)
		[t,txs] = multimes(A,n);
		r = mul(t,A(:,:,mod(k,s)+1));
		xs = [ txs mod(k,s)];
	else
		r = A(:,:,mod(k,s)+1);
		xs = mod(k,s);
	end
end
