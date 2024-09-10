        s    = dij / h
        s1   = 1.0_MK - s
        s2   = 2.0_MK - s
        s3   = 3.0_MK - s
        
        s1_4  = s1*s1*s1*s1
        s2_4  = s2*s2*s2*s2
        s3_4  = s3*s3*s3*s3
        
        s1_5  = 15.0_MK * s1_4 * s1
        s2_5  = -6.0_MK * s2_4 * s2
        s3_5  = s3_4 * s3
        
        s1_4  =  75.0_MK * s1_4
        s2_4  = -30.0_MK * s2_4
        s3_4  =  5.0_MK  * s3_4
        
        
        IF ( s < 1.0_MK) THEN
           
           w     = coef * (s3_5  + s2_5 +  s1_5)
           gradW = coef_grad * (s3_4 + s2_4 + s1_4)
           
        ELSE IF ( s < 2.0_MK ) THEN

           w     = coef * (s3_5 + s2_5 )
           gradW = coef_grad * (s3_4 + s2_4 )
           
        ELSE IF( s < 3.0_MK ) THEN
           
           w     = coef * s3_5
           gradW = coef_grad * s3_4
           
        ELSE
           
           w     = 0.0_MK
           gradW = 0.0_MK
           
        END IF
