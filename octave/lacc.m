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
