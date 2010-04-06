function [c,xs] = revmultimes(A,k)
	s = length(A(1,1,:));
	n = floor(k/s);
	if(n > 0)
		[t,txs] = revmultimes(A,n);
		c = mul(A(:,:,mod(k,s)+1),t);
		xs = [ mod(k,s) txs];
	else
		c = A(:,:,mod(k,s)+1);
		xs = mod(k,s);
	end
end
