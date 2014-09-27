(defparameter counter1 0)
(defparameter counter2 0)
(defparameter counter3 0)
(defparameter counter4 0)

(defparameter flag (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		))

(defun numbrix()
	
	(rules)
	(loop
		
		(setq flag (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		))

		(terpri) (terpri) (princ "Choose a board (1 to 17): ")
		(setq choice (read))

		(cond 	((equal choice 1) (setq brd (listcopy (setq o-brd (list '(1 - - - 9) 
										'(- - 6 - -) 
										'(- 18 - 14 -) 
										'(- - 16 - -) 
										'(21 - - - 25))))) 

					  (setq arr (sublistcopy (setq available (list 0 1 1 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 0))))

					  (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - -) 
											'(- - - - -) 
											'(- - - - -) 
											'(- - - - -) 
											'(- - - - -))))))		

			((equal choice 2) (setq brd (listcopy (setq o-brd (list '(3 4 9 10 15 16) 
										'(2 - - - - 17) 
										'(1 - - - - 18) 
										'(36 - - - - 19) 
										'(31 - - - - 24) 
										'(30 29 28 27 26 25)))))

					  (setq arr (sublistcopy (setq available (list 0 0 0 0 1 1 1 1 0 0 1 1 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0))))

			 		  (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - - -) 
			       	       				       			'(- - - - - -) 
			               				       			'(- - - - - -) 
			               				       			'(- - - - - -) 
			               				       			'(- - - - - -) 
			               				       			'(- - - - - -))))))
			
			((equal choice 3) (setq brd (listcopy (setq o-brd (list '(1 - 9 - 25 - 49) 
										'(- 3 - 11 - 27 -) 
										'(5 - - - - - 47) 
										'(- 15 - - - 29 -) 
										'(17 - - - - - 45) 
										'(- 35 - 33 - 31 -) 
										'(37 - 39 - 41 - 43)))))

					(setq arr (sublistcopy (setq available (list 0 1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - -) 
			       	       				       '(- - - - - - -) 
			               				       '(- - - - - - -) 
			               				       '(- - - - - - -) 
			               				       '(- - - - - - -) 
			               				       '(- - - - - - -)
								       '(- - - - - - -))))))

			((equal choice 4) (setq brd (listcopy (setq o-brd (list '(54 - 52 - - 37 - 35) 
										'(- - - - - - - -) 
										'(58 - 48 - - 41 - 31) 
										'(- - - 46 - - - -) 
										'(- - - - 16 - - -) 
										'(63 - 5 - - 18 - 26) 
										'(- - - - - - - -) 
										'(1 - 11 - - 20 - 22)))))

		(setq arr (sublistcopy (setq available (list 0 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 1 0 1 0 1 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - - -) 
			       	       				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -))))))

			((equal choice 5) (setq brd (listcopy (setq o-brd (list '(1 - - - 9) 
										'(- - - - -) 
										'(- - 21 - -) 
										'(- - - - -) 
										'(25 - - - 15)))))

			(setq arr (sublistcopy (setq available (list 	0 1 1 1 1 
			 						1 1 1 0 1 
			 						1 1 1 1 0 
			 						1 1 1 1 1 
		   	 						0 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - -) 
									'(- - - - -) 
									'(- - - - -) 
									'(- - - - -) 
									'(- - - - -))))))

			((equal choice 6) (setq brd (listcopy (setq o-brd (list '(25 - - - 13) 
										'(- - - - -) 
										'(- - 1 - -) 
										'(- - - - -) 
										'(21 - - - 17)))))

			(setq arr (sublistcopy (setq available (list 	0 1 1 1 1
			 						1 1 1 1 1 
			 						1 1 0 1 1 
			 						1 0 1 1 1 
			 						0 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - -) 
									'(- - - - -) 
									'(- - - - -) 
									'(- - - - -) 
									'(- - - - -))))))

			((equal choice 7) (setq brd (listcopy (setq o-brd (list '(49 - - - - - 31) 
										'(- 9 - 3 - - -) 
										'(- - - - - - -) 
										'(- - - 5 - - -) 
										'(- - - - 17 - -) 
										'(- - - - - 25 -) 
										'(- - - - - - 37)))))

		(setq arr (sublistcopy (setq available (list 	1 1 0 1 0 1 1 
			 					1 0 1 1 1 1 1 
			 					1 1 0 1 1 1 1 
			 					1 1 1 0 1 1 1 
			 					1 1 0 1 1 1 1 
			 					1 0 1 1 1 1 1 
			 					1 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - -) 
			       	       				       '(- - - - - - -) 
			               				       '(- - - - - - -) 
			               				       '(- - - - - - -) 
			               				       '(- - - - - - -) 
			               				       '(- - - - - - -)
								       '(- - - - - - -))))))

			((equal choice 8) (setq brd (listcopy (setq o-brd (list '(1 - - - - - - 64) 
										'(- 7 - - - - 38 -) 
										'(- - - 14 19 - - -) 
										'(- - - - - - - -) 
										'(- - - - - - - -) 
										'(- - - 29 30 - - -) 
										'(- 48 - - - - 43 -) 
										'(50 - - - - - - 57)))))

		(setq arr (sublistcopy (setq available (list 	0 1 1 1 1 1 0 1
			 					1 1 1 1 1 0 1 1 
			 					1 1 0 1 1 1 1 1 
			 					1 1 1 1 0 0 1 1 
			 					1 1 1 1 1 0 1 1 
			 					1 1 0 1 1 1 1 0
			 					1 0 1 1 1 1 1 1 
			 					0 1 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - - -) 
			       	       				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -) 
			               				       '(- - - - - - - -))))))

			((equal choice 9) (setq brd (listcopy (setq o-brd (list '(- - - - - - - - -) 
										'(- 75 - 9 - 3 - 43 -) 
										'(- - 79 - 1 - 15 - -) 
										'(- 77 - - - - - 41 -) 
										'(- - 21 - - - 17 - -) 
										'(- 67 - - - - - 39 -) 
										'(- - 31 - 29 - 27 - -) 
										'(- 63 - 33 - 35 - 37 -) 
										'(- - - - - - - - -)))))

		(setq arr (sublistcopy (setq available (list 	0 1 0 1 1 1 1 1 0
			 					1 1 1 1 1 0 1 0 1 
			 					1 1 0 1 1 1 1 1 0
			 					1 0 1 0 1 0 1 0 1
			 					0 1 0 1 0 1 0 1 1
			 					1 1 1 1 1 1 1 1 1 
			 					1 1 1 1 1 1 1 1 0
			 					1 1 1 0 1 1 1 1 1 
			 					1 1 0 1 0 1 0 1 1))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - - - -) 
			       	       				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -)
								       '(- - - - - - - - -))))))

			((equal choice 10) (setq brd (listcopy (setq o-brd (list '(73 - 81 - 11 - 13 - 45) 
										 '(- - - - - - - - -) 
										 '(- - - - 1 - - - -) 
										 '(- - - - - - - - -) 
										 '(- 68 - - 19 - - 40 -) 
										 '(- - - - - - 26 - -) 
										 '(- - - - 29 - - - -) 
										 '(- - 32 - - - 36 - -) 
										 '(61 - - - - - - - 53)))))

				(setq arr (sublistcopy (setq available (list   0 1 1 1 1 1 1 1 1 1 
									0 1 0 1 1 1 1 1 0 1
									1 1 1 1 1 0 1 1 0 1
									1 0 1 1 1 0 1 1 1 0
									1 1 1 1 0 1 1 1 1 1
									1 1 0 1 1 1 1 1 1 1
									0 1 1 1 1 1 1 0 1 1
									1 1 0 1 1 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - - - -) 
			       	       				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -)
								       '(- - - - - - - - -))))))

			((equal choice 11) (setq brd (listcopy (setq o-brd (list '(81 - 79 - - - - 74 - 72) 
										 '(- - - 15 - - 18 - - -) 
										 '(- - - - - - - - - -) 
										 '(- - - - - - - - - -) 
										 '(- - 91 - 1 - - - - 64) 
										 '(- - 32 - - 9 - - - -) 
										 '(- - - - - - 25 - - -) 
										 '(- - - - - - - 47 - -) 
										 '(- - - - - - - - 53 -) 
										 '(98 - 38 - - - - - - 59)))))

		(setq arr (sublistcopy (setq available (list 	0 1 1 1 1 1 1 1 0 1
			  					1 1 1 1 0 1 1 0 1 1
             		  					1 1 1 1 0 1 1 1 1 1 
 			  					1 0 1 1 1 1 1 0 1 1
			  					1 1 1 1 1 1 0 1 1 1
			  					1 1 0 1 1 1 1 1 0 1
			  					1 1 1 0 1 1 1 1 1 1 
			  					1 0 1 0 1 1 1 1 0 1 
			  					0 1 1 1 1 1 1 1 1 1 
			  					0 1 1 1 1 1 1 0 1 1))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - - - - - - -) 
			       	       				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -)
								       	'(- - - - - - - - - -)
									'(- - - - - - - - - -))))))

			((equal choice 12) (setq brd (listcopy (setq o-brd (list '(68 - - - - - - - - 77) 
										 '(- 50 - - - - - - - -) 
										 '(- - 20 - - - - 29 - -) 
										 '(- - - 22 - - - - - -) 
										 '(- - - - 10 - - - - -) 
										 '(- - - - 1 6 - - - -) 
										 '(- - - - - - 34 - - -) 
										 '(- - 15 - - - - 36 - -) 
										 '(- - - - - - - - 86 - ) 
										 '(59 - - - - - - - - 100)))))

			(setq arr (sublistcopy (setq available (list   0 1 1 1 1 0 1 1 1 0
								1 1 1 1 0 1 1 1 1 0
								1 0 1 1 1 1 1 1 0 1
								1 1 1 0 1 0 1 1 1 1
								1 1 1 1 1 1 1 1 1 0
								1 1 1 1 1 1 1 1 0 1
								1 1 1 1 1 1 1 0 1 1
								1 1 1 1 1 1 0 1 1 1
								1 1 1 1 1 0 1 1 1 1
								1 1 1 1 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - - - - - - -) 
			       	       				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -) 
			               				       	'(- - - - - - - - - -)
								       	'(- - - - - - - - - -)
									'(- - - - - - - - - -))))))

			((equal choice 13) (setq brd (listcopy (setq o-brd (list '(1 4 5 6 7 8 21 22 41 42 43) 
										 '(2 - - - - - - - - - 44) 
										 '(119 - 13 - - - - - - - 47) 
										 '(120 - - - 104 - - - - - 48) 
										 '(121 - - - - - - - - - 51) 
										 '(112 - - - - - - - - - 52) 
										 '(111 - - - - - - - 35 - 55) 
										 '(96 - - 99 - 83 - - - - 56) 
										 '(95 - - - - - - - - - 57 ) 
										 '(92 - - - - - - - - - 58) 
										 '(91 90 89 78 77 76 75 74 73 60 59)))))

		(setq arr (sublistcopy (setq available (list 	0 0 1 0 0 0 0 0 1 1 1 
			  					1 0 1 1 1 1 1 1 1 0 0
			  					1 1 1 1 1 1 1 1 1 1 1 
			  					1 0 1 1 1 1 1 0 0 0 0
			  					1 1 0 0 1 1 0 0 1 1 0 
			  					0 0 0 0 0 1 1 1 1 1 1
		          					1 1 1 1 1 1 0 0 0 0 0
			  					0 1 1 1 1 0 1 1 1 1 1 
			  					0 0 0 0 1 1 0 0 1 1 0
			  					1 1 1 1 0 1 1 1 1 1 1
			  					0 0 1 1 1 1 1 1 0 0 0))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - - - - - - - -) 
			       	       				       	'(- - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - -)
								       	'(- - - - - - - - - - -)
									'(- - - - - - - - - - -)
									'(- - - - - - - - - - -))))))

			((equal choice 14) (setq brd (listcopy (setq o-brd (list '(17 - 19 - 21 - 45 - 55 - 97 - 161) 
										 '(- 5 - 7 - 43 - 53 - 95 - 163 -) 
										 '(15 - - - - - - - - - - - 159) 
										 '(- 3 - - - - - - - - - 165 -) 
										 '(13 - - - - - - - - - - - 157) 
										 '(- 29 - - - - - 61 - - - 167 -) 
										 '(31 - - - - - - - - - - - 155) 
										 '(- 69 - - - - - - - - - 169 -) 
										 '(71 - - - - - - - - - - - 153 ) 
										 '(- 73 - - - - - 85 - - - 149 -) 
										 '(117 - - - - - - - - - - - 147) 
										 '(- 121 - 125 - 129 - 133 - 137 - 145 -) 
										 '(119 - 123 - 127 - 131 - 135 - 141 - 143)))))

				(setq arr (sublistcopy (setq available (list   1 1 0 1 0 1 0 1 1 1
									1 1 0 1 0 1 0 1 0 1
									0 1 1 1 1 1 1 1 0 1
									0 1 1 1 1 1 1 1 1 1
									1 1 0 1 0 1 1 1 1 1
									1 1 0 1 0 1 1 1 1 1
									0 1 1 1 1 1 1 1 0 1
									0 1 0 1 1 1 1 1 1 1
									1 1 1 1 0 1 1 1 1 1
									1 1 1 1 0 1 0 1 1 1
									1 1 1 1 1 1 1 1 1 1
									1 1 1 1 1 1 0 1 0 1
									0 1 0 1 0 1 0 1 0 1
									0 1 0 1 0 1 0 1 1 1
									0 1 0 1 0 1 0 1 0 1
									1 1 0 1 0 1 0 1 0 1
									0 1 0 1 0 1 0 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - - - - - - - - - -) 
			       	       				       	'(- - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - -)
								       	'(- - - - - - - - - - - - -)
									'(- - - - - - - - - - - - -)
									'(- - - - - - - - - - - - -)
									'(- - - - - - - - - - - - -)
									'(- - - - - - - - - - - - -))))))

			((equal choice 15) (setq brd (listcopy (setq o-brd (list '(41 - - - - - - - - - - - - - 211) 
										 '(- 43 - - - - - - - - - - - 77 -) 
										 '(- - 197 - - - - - - - - - - - -) 
										 '(- - - 89 - - - - - - - - - - -) 
										 '(- - - - 159 - - - - - 143 - - - -) 
										 '(- - - - - 119 - - - - - - - - -) 
										 '(- - - - - - 137 - - - - - - - -) 
										 '(- - - - - - - 135 - - - - - - -) 
										 '(- - - - - - 123 - 125 - - - - - -) 
										 '(- - - - - - - - - 149 - - - - -) 
										 '(- - - - 97 - - - - - 103 - - - -) 
										 '(- - - - - - - - - - - 179 - - -) 
										 '(- - - - - - - - - - - - 65 - -) 
										 '(- - - - - - - - - - - - - 1 -) 
										 '(27 - - - - - - - - - - - - - 225)))))

			(setq arr (sublistcopy (setq available (list 	0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
			  						1 1 1 1 1 1 1 1 1 1 1 0 1 1 1
			  						1 1 1 1 1 1 1 1 1 1 0 1 0 1 1
			  						1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
			  						1 1 1 1 0 1 1 1 1 1 1 1 1 1 1
			  						1 0 1 1 1 1 1 1 1 1 1 1 1 0 1
			  						1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 
			  						1 1 1 1 1 1 1 1 1 1 1 1 1 0 1
			  						1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 
			  						1 0 1 1 1 1 1 0 1 1 1 1 1 0 1
			  						1 1 1 1 1 1 1 1 0 1 1 1 1 1 1
			  						1 1 1 1 1 1 1 1 1 1 1 1 1 0 1
		    	  						1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
			  						1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
			  						0 1 1 1 1 1 1 1 1 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list 	'(- - - - - - - - - - - - - - -) 
			       	       				       	'(- - - - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - - - -) 
			               				       	'(- - - - - - - - - - - - - - -)
								       	'(- - - - - - - - - - - - - - -)
									'(- - - - - - - - - - - - - - -)
									'(- - - - - - - - - - - - - - -)
									'(- - - - - - - - - - - - - - -)
									'(- - - - - - - - - - - - - - -)
									'(- - - - - - - - - - - - - - -)
									'(- - - - - - - - - - - - - - -))))))

			((equal choice 16) (setq brd (listcopy (setq o-brd (list '(43 - 41 - 39 - 33 - 27) 
										 '(- - - - - - - - -) 
										 '(51 - - - - - - - 25) 
										 '(- - - - - - - - -) 
										 '(53 - - - - - - - 23) 
										 '(- - - - - - - - -) 
										 '(55 - - - - - - - 21) 
										 '(- - - - - - - - -) 
										 '(57 - 81 - 71 - 7 - 9)))))

				(setq arr (sublistcopy (setq available (list   1 1 1 1 1 1 0 1 0 1
									1 1 1 1 1 1 1 1 1 1
									0 1 0 1 0 1 0 1 1 1
									1 1 0 1 1 1 1 1 0 1
									0 1 0 1 1 1 1 1 1 1
									0 1 0 1 0 1 0 1 1 1
									1 1 1 1 1 1 1 1 1 1
									0 1 1 1 1 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - - - -) 
			       	       				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -)
								       '(- - - - - - - - -))))))

			((equal choice 17) (setq brd (listcopy (setq o-brd (list '(1 - 3 - 19 - 23 - 25) 
										 '(- - - - - - - - -) 
										 '(7 - - - - - - - 27) 
										 '(- - - - - - - - -) 
										 '(75 - - - - - - - 35) 
										 '(- - - - - - - - -) 
										 '(81 - - - - - - - 45) 
										 '(- - - - - - - - -) 
										 '(61 - 59 - 57 - 49 - 47)))))

			(setq arr (sublistcopy (setq available (list   0 1 0 1 1 1 0 1 1 1
								1 1 1 1 1 1 1 1 0 1
								1 1 0 1 0 1 0 1 1 1
								1 1 1 1 0 1 1 1 1 1
								1 1 1 1 0 1 0 1 0 1
								1 1 1 1 1 1 0 1 0 1
								0 1 1 1 1 1 1 1 1 1
								1 1 1 1 0 1 1 1 1 1 0))))

			   (setq empty-brd (listcopy (setq e-brd (list '(- - - - - - - - -) 
			       	       				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -) 
			               				       '(- - - - - - - - -)
								       '(- - - - - - - - -))))))
		)
		
		(loop
			(terpri) (terpri) (princ "For manual play press 1, for auto play press 2: ")
			(setq ans (read))
			(when (or (equal ans 1) (equal ans 2)) (return T))
		)

		(cond	((equal ans 1) (manual-play-numbrix brd o-brd empty-brd))
			(t (auto-play-numbrix brd empty-brd arr))
		)
		
		(loop
			(terpri) (terpri) (princ "Would you like to play the game again? (y/n) ")
			(setq ans (read))
			(when (or (equal ans 'y) (equal ans 'n)) (return T))
		)
		(when (equal ans 'n) (terpri) (princ "END OF GAME!") (return T))
	)
)

(defun rules()
	(princ "RULES:")
	(terpri) (princ "The objective of this game is to fill the board with sequence of consecutive numbers from 1 to n^2 for a nxn matrix.") 
	(terpri) (princ "The numbers must be placed in horizontal or vertical fashion (no diagonals).")
	(terpri) (princ "To win the game, the numbers must follow a snake-like structure throughout the board starting from 1 till the last number.")
)

(defun auto-play-numbrix(brd empty-brd arr)
	(p-board brd)

	(setq t1 (get-internal-run-time))		
	(loop	
		(loop
			(setq counter1 0)
			(setq counter2 0)
			(setq counter3 0)
			(one-direction-moves-possible brd arr)
			(alternate-number-present-check brd arr)
			(two-direction-moves-possible brd arr)
			(when (and (= counter1 0) (and (= counter2 0) (= counter3 0))) (return t))
		)

		(setq value 1)
		(setq n (list-length brd))
		(setq counter4 0)
		(loop
			(setq lst (distance-between-numbers value brd n))
			(setq a1 (car lst))
			(setq b1 (car (cdddr lst)))
			(cond 	((and (= a1 0) (= b1 0)) nil) 
				((= a1 0) nil)
				((= b1 0) nil)
				(t (find-possible-paths1 lst brd arr))
			)
			
			(setq value b1)
			(when (= value 0) (return t))
		)

		(when (and (= value 0) (and (= counter1 0) (and (= counter2 0) (= counter3 0)))) (return t))
	)

	(loop for i from 1 to n do
        (loop for j from 1 to n do
              	(cond	((equal (find-possible-paths2  i j 1 brd arr n) t) t)
                    	(t (continue))
		)
	))

	(setq t2 (get-internal-run-time))

	(setq t3 (- t2 t1))
	(setq t3 (float (/ t3 internal-time-units-per-second)))

	(terpri) (terpri) (princ "AI RESULT:")
	(terpri) (p-board brd)
	(terpri) (terpri) (princ "PATTERN DISPLAY:") 
	(terpri) (princ "----------------")
	(terpri) (traverse-snake-like-structure brd empty-brd)

	(terpri) (terpri) (princ "Time taken: ")
	(princ t3)
	(princ " secs")
)

(defun one-direction-moves-possible(brd arr)
	(setq n (list-length brd))
	(setq v 1)
	(loop
		(setq r (find-row-number v brd n))
		(setq c (find-col v brd))
		(setq x (and r c))
		(if 	(null x) (setq v (1+ v)) 
			(progn
				(if 	(= v 1) (progn
							(setq x (check-neighbor (1+ v) brd))
							(if (equal x t) (setq v (1+ v)) (progn (one-direction-moves-check v brd 0 (1+ v) arr) (setq v (1+ v))))
						)
				(progn
				(if 	(= v (* n n)) (progn
						      		(setq x (check-neighbor (1- v) brd))
								(if (equal x t) (setq v (1+ v)) (progn (one-direction-moves-check v brd (1- v) 0 arr) (setq v (1+ v))))
						      ) 
					(progn 
						(setq x (check-neighbor (1- v) brd))
						(setq y (check-neighbor (1+ v) brd))
						(if 	(or (equal (and x y) t) (and (null x) (null y))) (setq v (1+ v)) 
							(progn 
								(if (null x) (progn (one-direction-moves-check v brd (1- v) 0 arr) (setq v (1+ v)))
								(progn (one-direction-moves-check v brd 0 (1+ v) arr) (setq v (1+ v)))
								)
							)
						)
					)
				)
				))
			)
		)
		(when (> v (* n n)) (return t))	
	)	
)

(defun one-direction-moves-check(v brd x y arr)
	(setq n (list-length brd))
	(setq r (find-row-number v brd n))
	(setq c (find-col v brd))
	(setq f (check-up r c brd))
	(setq g (check-down r c brd))
	(setq h (check-left r c brd))
	(setq i (check-right r c brd))

	(cond 	((= x 0) (setq put y))
		(t (setq put x))
	)
	
	(cond 
		((and (equal f t) (and (equal g t) (and (equal h t) (null i)))) (set-sqr r (1+ c) brd put) (set-sqr1 put arr 0) (if (= counter1 0) (setq counter1 1)))
		((and (equal f t) (and (equal g t) (and (equal i t) (null h)))) (set-sqr r (1- c) brd put) (set-sqr1 put arr 0) (if (= counter1 0) (setq counter1 1)))
		((and (equal f t) (and (equal i t) (and (equal h t) (null g)))) (set-sqr (1- r) c brd put) (set-sqr1 put arr 0) (if (= counter1 0) (setq counter1 1)))
		((and (equal i t) (and (equal g t) (and (equal h t) (null f)))) (set-sqr (1+ r) c brd put) (set-sqr1 put arr 0) (if (= counter1 0) (setq counter1 1)))
		(t nil)
	)		
)

(defun alternate-number-present-check(brd arr)
	(setq n (list-length brd))
	(setq v 1)
	(loop
		(setq r (find-row-number v brd n))
		(setq c (find-col v brd))
		(setq x (and r c))
		(if 	(null x) (setq v (1+ v)) 
			(progn
				(setq x (check-neighbor (1+ v) brd))
				(if 	(equal x t) (setq v (1+ v)) 
					(progn 
						(setq x (check-neighbor (+ 2 v) brd))
						(if 	(null x) (setq v (+ 3 v))
							(progn
								(setq r2 (find-row-number (+ 2 v) brd n))
								(setq c2 (find-col (+ 2 v) brd))
								(alternate-number-present v r c (+ 2 v) r2 c2 brd arr)	
								(setq v (+ 2 v))	
							)
						)
					)
				)
			)
		)
		(when (> v (- (* n n) 2)) (return t))	
	)	
)

(defun alternate-number-present(v r c v2 r2 c2 brd arr)

	(cond 	((and (= r2 (1- r)) (= c2 (1+ c))) (setq x (sqr r (1+ c) brd)) (setq y (sqr (1- r) c brd))
			(if 	(and (equal x '-) (equal y '-)) nil
				(progn
					(if 	(equal x '-) (progn (set-sqr r (1+ c) brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
						(progn (set-sqr (1- r) c brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
					)
				)
			)	
		)
		
		((and (= r2 (1+ r)) (= c2 (1+ c))) (setq x (sqr r (1+ c) brd)) (setq y (sqr (1+ r) c brd))
			(if 	(and (equal x '-) (equal y '-)) nil
				(progn
					(if 	(equal x '-) (progn (set-sqr r (1+ c) brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
						(progn (set-sqr (1+ r) c brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
					)
				)
			) 
		)
		
		((and (= r2 (1- r)) (= c2 (1- c))) (setq x (sqr r (1- c) brd)) (setq y (sqr (1- r) c brd))
			(if 	(and (equal x '-) (equal y '-)) nil
				(progn
					(if 	(equal x '-) (progn (set-sqr r (1- c) brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
						(progn (set-sqr (1- r) c brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
					)
				)
			) 
		)
		
		((and (= r2 (1+ r)) (= c2 (1- c))) (setq x (sqr r (1- c) brd)) (setq y (sqr (1+ r) c brd))
			(if 	(and (equal x '-) (equal y '-)) nil
				(progn
					(if 	(equal x '-) (progn (set-sqr r (1- c) brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
						(progn (set-sqr (1+ r) c brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
					)
				)
			)  
		)

		
		((and (= r2 r) (= c2 (- c 2))) (set-sqr r (1- c) brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
		((and (= r2 r) (= c2 (+ c 2))) (set-sqr r (1+ c) brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
		((and (= r2 (- r 2)) (= c2 c)) (set-sqr (1- r) c brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
		((and (= r2 (+ r 2)) (= c2 c)) (set-sqr (1+ r) c brd (1+ v)) (set-sqr1 (1+ v) arr 0) (if (= counter2 0) (setq counter2 1)))
		(t nil)		
	)
	
)

(defun two-direction-moves-possible(brd arr)
	(setq n (list-length brd))
	(setq v 1)
	(loop
		(setq r (find-row-number v brd n))
		(setq c (find-col v brd))
		(setq x (and r c))
		(if 	(null x) (setq v (1+ v)) 
			(progn
				(if 	(= v 1) (progn
							(setq x (check-neighbor (1+ v) brd))
							(if (equal x t) (setq v (1+ v)) (progn (two-direction-moves-check v brd 0 (1+ v) r c arr) (setq v (1+ v))))
						)
				(progn
				(if 	(= v (* n n)) (progn
						      		(setq x (check-neighbor (1- v) brd))
								(if (equal x t) (setq v (1+ v)) (progn (two-direction-moves-check v brd (1- v) 0 r c arr) (setq v (1+ v))))
						      ) 
					(progn 
						(setq x (check-neighbor (1- v) brd))
						(setq y (check-neighbor (1+ v) brd))
						(if 	(or (equal (and x y) t) (and (null x) (null y))) (setq v (1+ v)) 
							(progn 
								(if (null x) (progn (two-direction-moves-check v brd (1- v) 0 r c arr) (setq v (1+ v)))
								(progn (two-direction-moves-check v brd 0 (1+ v) r c arr) (setq v (1+ v)))
								)
							)
						)
					)
				)
				))
			)
		)
		(when (> v (* n n)) (return nil))	
	)	
)

(defun two-direction-moves-check(v brd x y r c arr)
	(setq n (list-length brd))
	(setq f (check-up r c brd))
	(setq g (check-down r c brd))
	(setq h (check-left r c brd))
	(setq i (check-right r c brd))

	(cond 	((= x 0) (setq put y))
		(t (setq put x))
	)
	
	(cond 
		((and (equal f t) (and (equal h t) (and (null g) (null i)))) (setq x (down-check r c brd)) (setq y (right-check r c brd))
			(if 	(or (and (equal x t) (null y)) (and (null x) (equal y t))) 
				(progn
					(if (equal x t) (set-sqr (1- r) c brd put) (set-sqr r (1+ c) brd put))
					(if (= counter3 0) (setq counter3 1))
					(set-sqr1 put arr 0)
				)
			)
		)
		
		((and (equal g t) (and (equal h t) (and (null f) (null i)))) (setq x (up-check r c brd)) (setq y (right-check r c brd))
			(if 	(or (and (equal x t) (null y)) (and (null x) (equal y t))) 
				(progn
					(if (equal x t) (set-sqr (1+ r) c brd put) (set-sqr r (1+ c) brd put))
					(if (= counter3 0) (setq counter3 1))
					(set-sqr1 put arr 0)
				)
			)
		)

		((and (equal i t) (and (equal g t) (and (null h) (null f)))) (setq x (up-check r c brd)) (setq y (left-check r c brd))
			(if 	(or (and (equal x t) (null y)) (and (null x) (equal y t))) 
				(progn
					(if (equal x t) (set-sqr (1+ r) c brd put) (set-sqr r (1- c) brd put))
					(if (= counter3 0) (setq counter3 1))
					(set-sqr1 put arr 0)
				)
			)
		)

		((and (equal i t) (and (equal f t) (and (null g) (null h)))) (setq x (down-check r c brd)) (setq y (left-check r c brd))
			(if 	(or (and (equal x t) (null y)) (and (null x) (equal y t))) 
				(progn
					(if (equal x t) (set-sqr (1- r) c brd put) (set-sqr r (1- c) brd put))
					(if (= counter3 0) (setq counter3 1))
					(set-sqr1 put arr 0)
				)
			)
		)
		(t nil)

		;; ((and (equal f t) (and (equal g t) (and (null h) (null i)))) (left-check r c brd) (right-check r c brd))
		;; ((and (equal i t) (and (equal h t) (and (null g) (null f)))) (up-check r c brd) (down-check r c brd))

	)		
)

(defun left-check(r c brd) 
	(setq n (list-length brd))
	(setq a (1- c))
	(setq count-blank 0)
	(setq temp 0)
	(setq b (1- a))
	(setq d (1- r))
	(setq e (1+ r))

	(if (or (< b 1) (or (< d 1) (> e n))) nil
	(progn
	(cond	((and (= r 1) (= a 1)) t)
		((and (= r n) (= a n)) t)
		((and (= r n) (= a 1)) t)
		((and (= r 1) (= a n)) t)
		(t 	
			(setq vl (sqr r b brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr d a brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr e a brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(cond	((and (= count-blank 1) (= temp 2)) t)
				(t nil)
			)
		)	
	)
	))	
)

(defun right-check(r c brd) 
	(setq n (list-length brd))
	(setq a (1+ c))
	(setq count-blank 0)
	(setq temp 0)
	(setq b (1+ a))
	(setq d (1- r))
	(setq e (1+ r))

	(if (or (> b n) (or (< d 1) (> e n))) nil
	(progn
	(cond	((and (= r 1) (= a 1)) t)
		((and (= r n) (= a n)) t)
		((and (= r n) (= a 1)) t)
		((and (= r 1) (= a n)) t)
		(t 	
			(setq vl (sqr r b brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr d a brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr e a brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(cond	((and (= count-blank 1) (= temp 2)) t)
				(t nil)
			)
		)	
	)
	))	
)

(defun down-check(r c brd) 
	(setq n (list-length brd))
	(setq a (1- r))
	(setq count-blank 0)
	(setq temp 0)
	(setq b (1- a))
	(setq d (1- c))
	(setq e (1+ c))

	(if (or (< b 1) (or (< d 1) (> e n))) nil
	(progn
	(cond	
		(t 	
			(setq vl (sqr b c brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr a d brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr a e brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(cond	((and (= count-blank 1) (= temp 2)) t)
				(t nil)
			)
		)	
	)
	))	
)

(defun up-check(r c brd) 
	(setq n (list-length brd))
	(setq a (1+ r))
	(setq count-blank 0)
	(setq temp 0)
	(setq b (1+ a))
	(setq d (1- c))
	(setq e (1+ c))
	
	(if (or (> b n) (or (< d 1) (> e n))) nil
	(progn
	(cond	((and (= r 1) (= a 1)) t)
		((and (= r n) (= a n)) t)
		((and (= r n) (= a 1)) t)
		((and (= r 1) (= a n)) t)
		(t 	
			(setq vl (sqr b c brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr a d brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(setq vl (sqr a e brd))
			(cond	((equal vl '-) (setq count-blank (1+ count-blank)))
				((equal vl 1) (setq x (check-neighbor (1+ vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				((equal vl (* n n)) (setq x (check-neighbor (1- vl) brd)) (if (equal x t) (setq temp (1+ temp))))
				(t (setq x (check-neighbor (1- vl) brd)) (setq y (check-neighbor (1+ vl) brd))
					(if (and (equal x t) (equal y t)) (setq temp (1+ temp)))
				)
			)

			(cond	((and (= count-blank 1) (= temp 2)) t)
				(t nil)
			)
		)	
	)
	))	
)

(defun distance-between-numbers(v brd n)

	(setq dist 0)	
	(if (= v 1) (setq x (check-neighbor v brd)))
	
	(cond 	((and (= v 1) (null x)) (setq v1 1)
					(loop
						(setq v1 (1+ v1))
						(setq x (check-neighbor v1 brd))
						(if (null x) (setq dist (1+ dist)))
						(when (or (equal x t) (= v1 (* n n))) (return t))	
					)

					(setq r1 (find-row-number v1 brd n))
					(setq c1 (find-col v1 brd))
					(setq dist (- v1 v))
					(setq lst (list 0 0 0 v1 r1 c1 dist brd))
		)

		(t
			(loop
				(setq x (check-neighbor (1+ v) brd))
				(if (equal x t) (setq v (1+ v)))
				(when (or (null x) (= v (* n n))) (return t))	
			)


			(cond 	((= v (* n n)) (setq lst (list 0 0 0 0 0 0 0 brd)))
				(t
					(setq r (find-row-number v brd n))
					(setq c (find-col v brd))
					(setq v1 v)		
					(loop
						(setq v1 (1+ v1))
						(setq x (check-neighbor v1 brd))
						(if (null x) (setq dist (1+ dist)))
						(when (or (equal x t) (= v1 (* n n))) (return t))	
					)

					(setq r1 (find-row-number v1 brd n))
					(setq c1 (find-col v1 brd))
		
					(if (and (= v1 (* n n)) (null x)) (setq lst (list v r c 0 0 0 dist brd))
						(progn
							(setq dist (1- (- v1 v)))
							(setq lst (list v r c v1 r1 c1 dist brd))
						)
					)
				)
			)		
		)
	)
)

(defun find-possible-paths1(lst brd arr)

	(setq num1 (car lst))
	(setq row1 (cadr lst))
	(setq col1 (caddr lst))
	(setq num2 (car (cdddr lst)))
	(setq row2 (cadr (cdddr lst)))
	(setq col2 (caddr (cdddr lst)))
	(setq dist (car (cdddr (cdddr lst))))

	
	(cond 	((and (= row1 row2) (and (< col2 col1) (= dist (1- (- col1 col2))))) 
			(setq cl col1)
			(setq num num1)
			(loop
				(set-sqr row1 (1- cl) brd (1+ num))
				(set-sqr1 (1+ num) arr 0)
				(setq cl (1- cl))
				(setq num (1+ num))
				(setq dist (1- dist))
				(when (= dist 0) (return t))
			)
			(if (= counter4 0) (setq counter4 1))
		)
		((and (= row1 row2) (and (< col1 col2) (= dist (1- (- col2 col1))))) 
			(setq cl col1)
			(setq num num1)
			(loop
				(set-sqr row1 (1+ cl) brd (1+ num))
				(set-sqr1 (1+ num) arr 0)
				(setq cl (1+ cl))
				(setq num (1+ num))
				(setq dist (1- dist))
				(when (= dist 0) (return t))
			)
			(if (= counter4 0) (setq counter4 1))
		)
		((and (= col1 col2) (and (< row2 row1) (= dist (1- (- row1 row2))))) 
			(setq rw row1)
			(setq num num1)
			(loop
				(set-sqr (1- rw) col1 brd (1+ num))
				(set-sqr1 (1+ num) arr 0)
				(setq rw (1- rw))
				(setq num (1+ num))
				(setq dist (1- dist))
 				(when (= dist 0) (return t))
			)
			(if (= counter4 0) (setq counter4 1))
		)
		((and (= col1 col2) (and (< row1 row2) (= dist (1- (- row2 row1))))) 
			(setq rw row1)
			(setq num num1)
			(loop
				(set-sqr (1+ rw) col1 brd (1+ num))
				(set-sqr1 (1+ num) arr 0)
				(setq rw (1+ rw))
				(setq num (1+ num))
				(setq dist (1- dist))
				(when (= dist 0) (return t))
			)
			(if (= counter4 0) (setq counter4 1))
		)

		((and (and (= dist 2) (= row1 row2)) (or (= col2 (1+ col1)) (= col2 (1- col1)))) 
			(if (> (1+ row1) n) (progn (set-sqr (1- row1) col1 brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr (1- row1) col2 brd (1- num2)) (if (= counter4 0) (set-sqr1 (1- num2) arr 0) (setq counter4 1))))
			(if (< (1- row1) 1) (progn (set-sqr (1+ row1) col1 brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr (1+ row1) col2 brd (1- num2)) (if (= counter4 0) (set-sqr1 (1- num2) arr 0) (setq counter4 1)))
			
			(progn
			
			(setq u1 (sqr (1+ row1) col1 brd))
			(setq u2 (sqr (1+ row1) col2 brd))
			(setq d1 (sqr (1- row1) col1 brd))
			(setq d2 (sqr (1- row1) col2 brd))

			(cond	((and (and (equal u1 '-) (equal u2 '-)) (and (equal d1 '-) (equal d2 '-))) nil)
				((and (equal u1 '-) (equal u2 '-)) (set-sqr (1+ row1) col1 brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr (1+ row1) col2 brd (1- num2)) (set-sqr1 (1- num2) arr 0) (if (= counter4 0) (setq counter4 1)))
				((and (equal d1 '-) (equal d2 '-)) (set-sqr (1- row1) col1 brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr (1- row1) col2 brd (1- num2)) (set-sqr1 (1- num2) arr 0) (if (= counter4 0) (setq counter4 1)))
			)
			))
		)
		((and (and (= dist 2) (= col1 col2)) (or (= row2 (1+ row1)) (= row2 (1- row1))))

			(if (> (1+ col1) n) (progn (set-sqr row1 (1- col1) brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr row2 (1- col1) brd (1- num2)) (set-sqr1 (1- num2) arr 0) (if (= counter4 0) (setq counter4 1))))
			(if (< (1- col1) 1) (progn (set-sqr row1 (1+ col1) brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr row2 (1+ col1) brd (1- num2)) (set-sqr1 (1- num2) arr 0) (if (= counter4 0) (setq counter4 1)))
			
			(progn
		 	(setq l1 (sqr row1 (1- col1) brd))
			(setq l2 (sqr row2 (1- col1) brd))
			(setq r1 (sqr row1 (1+ col1) brd))
			(setq r2 (sqr row2 (1+ col1) brd))

			(cond	((and (and (equal l1 '-) (equal l2 '-)) (and (equal r1 '-) (equal r2 '-))) nil)
				((and (equal l1 '-) (equal l2 '-)) (set-sqr row1 (1- col1) brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr row2 (1- col1) brd (1- num2)) (set-sqr1 (1- num2) arr 0) (if (= counter4 0) (setq counter4 1)))
				((and (equal r1 '-) (equal r2 '-)) (set-sqr row1 (1+ col1) brd (1+ num1)) (set-sqr1 (1+ num1) arr 0) (set-sqr row2 (1+ col1) brd (1- num2)) (set-sqr1 (1- num2) arr 0) (if (= counter4 0) (setq counter4 1)))
			)
			))
		)
	)
)

(defun check-neighbor(val brd)
	(cond ((null brd) nil)
	((member val (car brd)) t)
	(t (check-neighbor val (cdr brd))))	
)

(defun find-row-number (v brd n)
	(cond ((null brd) nil)
	((member v (car brd)) n)
	(t (setq n (1- n)) (find-row-number v (cdr brd) n))
))

(defun find-col (v brd)
	(cond ((null brd) nil)
	((member v (car brd)) (setq lst (car brd)) (find-col-number v 0 lst))
	(t (find-col v (cdr brd)))
))

(defun find-col-number (v l lst)
	(cond	((null lst) nil)
		((equal v (car lst)) (1+ l))
		(t (find-col-number v (1+ l) (cdr lst)))
))

(defun check-up(row col brd)
	(setq n (list-length brd))
	(cond 	((> (1+ row) n) t)
		((equal (sqr (1+ row) col brd) '-) nil) 
		(t t)
	)
)

(defun check-down(row col brd)
	(cond 	((< (1- row) 1) t)
		((equal (sqr (1- row) col brd) '-) nil)
		(t t)
	)
)

(defun check-left(row col brd)
	(cond 	((< (1- col) 1) t)
		((equal (sqr row (1- col) brd) '-) nil) 
		(t t)
	)
)

(defun check-right(row col brd)
	(cond 	((> (1+ col) n) t)
		((equal (sqr row (1+ col) brd) '-) nil) 
		(t t)
	)
)

(defun find-possible-paths2(i j num brd arr n)

	(cond	((> num (* n n)) (return-from find-possible-paths2 t))
		(t (continue))
	)

	(cond
		((equal (sqr1 num arr) 0) (cond 	((equal (sqr i j brd) num) (continue))
							(t (return-from find-possible-paths2 nil))
						)
		)

		(t 	(cond 	((equal (sqr i j brd) '-) (continue))
				(t (return-from find-possible-paths2 nil))
			)

			(set-sqr i j brd num)
			(set-sqr1 num arr 0)
			(set-sqr1 num flag 1)
		)
	)

	(cond 	((and (< j n) (find-possible-paths2 i (1+ j) (1+ num) brd arr n)) (return-from find-possible-paths2 t))
		(t (continue))
	)
	(cond 	((and (> j 1) (find-possible-paths2 i (1- j) (1+ num) brd arr n)) (return-from find-possible-paths2 t))
		(t (continue))
	)
	(cond 	((and (< i n) (find-possible-paths2 (1+ i) j (1+ num) brd arr n)) (return-from find-possible-paths2 t))
		(t (continue))
	)
	(cond 	((and (> i 1) (find-possible-paths2 (1- i) j (1+ num) brd arr n)) (return-from find-possible-paths2 t))
		(t (continue))
	)

	(cond	((equal (sqr1 num flag) 1) (set-sqr i j brd '-) (set-sqr1 num arr 1) (set-sqr1 num flag 0)))
	
	(cond 	((null (member 1 arr)) (return-from find-possible-paths2 t))
		(t (return-from find-possible-paths2 nil)))
)

(defun sqr1 (col brd)
	(nth (1- col) brd))

(defun set-sqr1 (col brd val)
	(setf (nth (1- col) brd)
		val))

(defun manual-play-numbrix(brd o-brd empty-brd)	
	(p-board brd)
	(play brd o-brd)
	(terpri) (terpri) (princ "PATTERN DISPLAY:") 
	(terpri) (princ "----------------")
	(terpri) (traverse-snake-like-structure brd empty-brd)
)

(defun listcopy(original-brd)
	(cond 	((null original-brd) nil)
		(t (cons (sublistcopy (car original-brd)) (listcopy (cdr original-brd))))
	)	
)

(defun sublistcopy (brd)
	(cond 	((null brd) nil)
		(t (cons (car brd) (sublistcopy (cdr brd))))
	)
)

(defun play(brd o-brd)
	(terpri) (terpri) (terpri) (terpri) (princ "FILL A CELL")
	(terpri) (princ "-----------")
	(terpri) (terpri) (princ "Enter row, column and number: ")
	(setq row (read))
	(setq col (read))
	(setq val (read))
	(setq w (row-column-out-of-bound-check row col brd))
	(setq x (number-out-of-bound-check val brd))
	(setq y (cond ((and w x) (over-writing-cell-check row col brd o-brd))))
	(setq z	(cond ((equal y t) (repeating-number-check val brd))))
	
	(cond 	((not (and (and (and w x) y) z)) (play brd o-brd))
		(t (set-sqr row col brd val) (p-board brd)))

	(repeat-play brd o-brd)

	(terpri)
)


(defun row-column-out-of-bound-check(row col brd)
	(setq n (list-length brd))
	(cond	((and (or (> row n) (< row 0)) (or (> col n) (< col 0))) (terpri) (princ "ERROR: Row and column values out of bound") nil)
		((or (> row n) (< row 0)) (terpri) (princ "ERROR: Row value out of bound") nil)
		((or (> col n) (< col 0)) (terpri) (princ "ERROR: Column value out of bound") nil)
		(t t)
))

(defun number-out-of-bound-check (val brd)
	(setq n (list-length brd))
	(setq n (* n n))
	(cond 	((> val n) (terpri) (princ "ERROR: Number out of bound. Cannot have a value greater than ") (princ n) nil)
		((< val 1) (terpri) (princ "ERROR: Number out of bound. Cannot have a value smaller than 1") nil)
		(t t)
))

(defun over-writing-cell-check (row col brd o-brd)
	(setq q (sqr row col brd))
	(cond	((equal q '-) t)
		(t 
			(cond	((null (setq x (repeating-number-check1 q o-brd))) (terpri) (princ "ERROR: Cell (") (princ row) (princ ",") (princ col) (princ ") is already filled. Cannot over-write") nil)
				(t 	(loop
						(terpri) (princ "Are you sure you want to overwrite? (y/n) ")
						(setq ans (read))
						(when (or (equal ans 'y) (equal ans 'n)) (return T))
					)
					
					(cond 	((equal 'n ans) nil)
						(t t)
					)
				)
			) 
		)
))

(defun repeating-number-check (val brd)
	(cond 	((null brd) t)
		((member val (car brd)) (terpri) (princ "ERROR: ") (princ val) (princ " already present in the matrix") nil) 
		(t (repeating-number-check val (cdr brd)))
))

(defun repeating-number-check1 (val brd)
	(cond 	((null brd) t)
		((member val (car brd)) nil) 
		(t (repeating-number-check val (cdr brd)))
))

(defun repeat-play (brd o-brd)
	(setq x (repeat brd))
	(cond   ((null x) nil)
		(t (play brd o-brd))
))

(defun repeat (x)
	(cond 	((null x) nil)
		((member '- (car x)) t)
		(t (repeat (cdr x)))
))

(defun traverse-snake-like-structure (brd empty-brd)
	(setq row (find-1-row (list-length brd) brd))
	(setq col (find-1-col brd))
	(setq x (and row col))

	(if (null x) (progn (terpri) (princ "Cannot find 1 on the board! Incorrect Solution!"))
	
	(progn
		(set-sqr row col empty-brd 1)
		(p-board empty-brd)
		(go-up 1 row col brd empty-brd)
	))
)

(defun find-1-row (n brd)
	(cond ((null brd) nil)
	((member 1 (car brd)) n)
	(t (setq n (1- n)) (find-1-row n (cdr brd)))
))

(defun find-1-col (brd)
	(cond ((null brd) nil)
	((member 1 (car brd)) (setq lst (car brd)) (find-1-col-number 0 lst))
	(t (find-1-col (cdr brd)))
))

(defun find-1-col-number (l lst)
	(cond	((null lst) nil)
		((equal 1 (car lst)) (1+ l))
		(t (find-1-col-number (1+ l) (cdr lst)))
))

(defun go-up (val row col brd empty-brd)
	(setq n (list-length brd))
	(setq max (* n n))
	(if (>= val max) (progn (terpri) (princ "HOORAY! CORRECT SOLUTION!"))
		(if (> (1+ row) n) (go-down val row col brd empty-brd) 
			  (if (not (equal (sqr (1+ row) col brd) (1+ val))) (go-down val row col brd empty-brd)
			  (progn (setq row (1+ row)) (setq val (1+ val)) (set-sqr row col empty-brd val) (terpri) (p-board empty-brd) (go-up val row col brd empty-brd))	
			)     			
		)
))

(defun go-down (val row col brd empty-brd)
	(if (< (1- row) 1) (go-right val row col brd empty-brd) 
		(if (not (equal (sqr (1- row) col brd) (1+ val))) (go-right val row col brd empty-brd) 
		(progn (setq row (1- row)) (setq val (1+ val)) (set-sqr row col empty-brd val) (terpri) (p-board empty-brd) (go-up val row col brd empty-brd))
		)
	)
)

(defun go-right (val row col brd empty-brd)
	(if (> (1+ col) n) (go-left val row col brd empty-brd) 
		(if (not (equal (sqr row (1+ col) brd) (1+ val))) (go-left val row col brd empty-brd) 
		(progn (setq col (1+ col)) (setq val (1+ val)) (set-sqr row col empty-brd val) (terpri) (p-board empty-brd) (go-up val row col brd empty-brd))
		)
	)
)

(defun go-left (val row col brd empty-brd)
	(if (< (1- col) 1) (progn (terpri) (princ "SOLUTION INCORRECT! PLEASE TRY AGAIN!"))  
		(if (not (equal (sqr row (1- col) brd) (1+ val))) (progn (terpri) (princ "SOLUTION INCORRECT! PLEASE TRY AGAIN!")) 
		(progn (setq col (1- col)) (setq val (1+ val)) (set-sqr row col empty-brd val) (terpri) (p-board empty-brd) (go-up val row col brd empty-brd))
		)
	)
)

(defun sqr (row col brd)
	(setq n (list-length brd))
	(nth (1- col)
		(nth (- n row) brd )))

(defun set-sqr (row col brd val)
	(setq n (list-length brd))
	(setf (nth (1- col)
		(nth (- n row) brd))
		val))

(defun p-board (brd)
	(setq n (list-length brd))
	(cond 	((<= n 3) (p-board-single-digit brd n n))
		((<= n 9) (p-board-double-digit brd n n))
		(T (p-board-triple-digit brd n n))))

(defun p-board-single-digit (brd n count)	
	(cond 	((null brd) (terpri) (princ "     +") (print-plus-minus-single n) (print-index n n) (terpri))
		(T (terpri) (princ "     +") (print-plus-minus-single n)
		(terpri)
		(setq counter 0)
		(setq counter (check-number-of-digits counter count))
		(if (= counter 1) (progn (princ "  ") (princ count) (princ "  "))
			(progn (princ " ") (princ count) (princ "  "))
		)
		(p-row-single (car brd))
		(setq count (1- count))
		(p-board-single-digit (cdr brd) n count))))

(defun print-plus-minus-single (n)
	(cond 	((equal n 0) nil)
		(T (princ "-+") (print-plus-minus-single (1- n)))))

(defun p-row-single (row)
	(cond 	((null row) (princ "|"))
		(T (princ "|")
		(princ (car row))
		(p-row-single (cdr row)))))

(defun p-board-double-digit (brd n count)	
	(cond 	((null brd) (terpri) (princ "     +") (print-plus-minus-double n) (print-index n n) (terpri))
		(T (terpri) (princ "     +") (print-plus-minus-double n)
		(terpri)
		(setq counter 0)
		(setq counter (check-number-of-digits counter count))
		(if (= counter 1) (progn (princ "  ") (princ count) (princ "  "))
			(progn (princ " ") (princ count) (princ "  "))
		)
		(p-row-double (car brd))
		(setq count (1- count))
		(p-board-double-digit (cdr brd) n count))))

(defun print-plus-minus-double (n)
	(cond 	((equal n 0) nil)
		(T (princ "--+") (print-plus-minus-double (1- n)))))

(defun p-row-double (row)
	(cond 	((null row) (princ "|"))
		(T (princ "|")
		   (setq num (car row))
		   (princ num)
		   (setq counter 0)
		   (if (equal num '-) (princ " ") 
              	       (progn
		       (setq counter (check-number-of-digits counter num))
		       (if (= counter 1) (princ " ")) 
                       ))			
		(p-row-double (cdr row)))))

(defun p-board-triple-digit (brd n count)	
	(cond 	((null brd) (terpri) (princ "     +") (print-plus-minus-triple n) (print-index n n) (terpri))
		(T (terpri) (princ "     +") (print-plus-minus-triple n)
		(terpri)
		(setq counter 0)
		(setq counter (check-number-of-digits counter count))
		(if (= counter 1) (progn (princ "  ") (princ count) (princ "  "))
			(progn (princ " ") (princ count) (princ "  "))
		)
		(p-row-triple (car brd))
		(setq count (1- count))
		(p-board-triple-digit (cdr brd) n count))))

(defun print-plus-minus-triple (n)
	(cond 	((equal n 0) nil)
		(T (princ "---+") (print-plus-minus-triple (1- n)))))

(defun p-row-triple (row)
	(cond 	((null row) (princ "|"))
		(T (princ "|")
		   (setq num (car row))
		   (setq num1 num)
		   (setq counter 0)
		   (if (equal num '-) (princ " - ") 
              	   (progn
			(setq counter (check-number-of-digits counter num1))
			(if (= counter 1) (progn (princ " ") (princ num) (princ " "))
			(if (= counter 2) (progn (princ num) (princ " "))
			(princ num)))
                   ))			
		(p-row-triple (cdr row)))))

(defun print-index(ct n)
	(cond ((and (not (= ct 0)) (<= n 3)) 
		(cond	((= ct n) (terpri) (terpri) (princ "      ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
			(t (princ " ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
		))

		((and (not (= ct 0)) (<= n 9)) 
		(cond	((= ct n) (terpri) (terpri) (princ "      ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
			(t (princ "  ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
		))

		((not (= ct 0)) 
		(cond	((= ct n) (terpri) (terpri) (princ "      ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
			(t (cond
				((= (check-number-of-digits 0 (+ (- n ct) 1)) 1) (princ "   ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
				(t (princ "  ") (princ (+ (- n ct) 1)) (print-index (1- ct) n))
			   )
			)
		))
	)
)

(defun check-number-of-digits(counter num)
	(if (= num 0) counter
	(progn
		(setq num (floor (/ num 10)))
        	(setq counter (1+ counter))
		(check-number-of-digits counter num))))