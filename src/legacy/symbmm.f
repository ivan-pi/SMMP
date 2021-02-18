        subroutine symbmm
     *                   (n, m, l,
     *                    ia, ja, diaga,
     *                    ib, jb, diagb,
     *                    ic, jc, diagc,
     *                    index)
c
            integer       ia(*), ja(*), diaga,
     *                    ib(*), jb(*), diagb,
     *                    ic(*), jc(*), diagc,
     *                    index(*)
c
c       symbolic matrix multiply c=a*b
c
        maxlmn = max(l,m,n)
        do 10 i=1,maxlmn
   10       index(i)=0
        if (diagc.eq.0) then
            ic(1)=1
        else
            ic(1)=n+2
        endif
        minlm = min(l,m)
        minmn = min(m,n)
c
c    main loop
c
        do 50 i=1,n
            istart=-1
            length=0
c
c    merge row lists
c
            do 30 jj=ia(i),ia(i+1)
c    a = d + ...
                if (jj.eq.ia(i+1)) then
                    if (diaga.eq.0 .or. i.gt.minmn) goto 30
                    j = i
                else
                    j=ja(jj)
                endif
c    b = d + ...
                if (index(j).eq.0 .and. diagb.eq.1 .and. j.le.minlm)then
                    index(j)=istart
                    istart=j
                    length=length+1
                endif
                do 20 k=ib(j),ib(j+1)-1
                    if(index(jb(k)).eq.0) then
                        index(jb(k))=istart
                        istart=jb(k)
                        length=length+1
                    endif
   20           continue
   30       continue
c
c   row i of jc
c
            if (diagc.eq.1 .and. index(i).ne.0) length = length - 1
            ic(i+1)=ic(i)+length
            do 40 j= ic(i),ic(i+1)-1
                if (diagc.eq.1 .and. istart.eq.i) then
                    istart = index(istart)
                    index(i) = 0
                endif
                jc(j)=istart
                istart=index(istart)
                index(jc(j))=0
   40       continue
            index(i) = 0
   50   continue
        return
        end