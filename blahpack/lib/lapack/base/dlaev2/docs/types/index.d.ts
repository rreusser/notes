

// TypeScript declarations for @stdlib/lapack/base/dlaev2

/**
* Result of dlaev2.
*/
interface Dlaev2Result {
	/**
	* Eigenvalue of larger absolute value.
	*/
	rt1: number;

	/**
	* Eigenvalue of smaller absolute value.
	*/
	rt2: number;

	/**
	* Cosine of the rotation.
	*/
	cs1: number;

	/**
	* Sine of the rotation.
	*/
	sn1: number;
}

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the eigendecomposition of a 2-by-2 symmetric matrix.
	*
	* @param a - (1,1) element of the 2-by-2 matrix
	* @param b - (1,2) element of the 2-by-2 matrix
	* @param c - (2,2) element of the 2-by-2 matrix
	* @returns object with rt1, rt2, cs1, and sn1 properties
	*/
	( a: number, b: number, c: number ): Dlaev2Result;

	/**
	* Computes the eigendecomposition of a 2-by-2 symmetric matrix (ndarray interface).
	*
	* @param a - (1,1) element of the 2-by-2 matrix
	* @param b - (1,2) element of the 2-by-2 matrix
	* @param c - (2,2) element of the 2-by-2 matrix
	* @returns object with rt1, rt2, cs1, and sn1 properties
	*/
	ndarray( a: number, b: number, c: number ): Dlaev2Result;
}

/**
* Computes the eigendecomposition of a 2-by-2 symmetric matrix.
*/
declare var dlaev2: Routine;

export = dlaev2;
