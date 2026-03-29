

// TypeScript declarations for @stdlib/lapack/base/dlarrc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Count eigenvalues of a symmetric tridiagonal matrix in an interval
	*/
	(
		jobt: string,
		N: number,
		vl: number,
		vu: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		pivmin: number,
		eigcnt: number,
		lcnt: number,
		rcnt: number
	): Float64Array;
}

/**
* Count eigenvalues of a symmetric tridiagonal matrix in an interval
*/
declare var dlarrc: Routine;

export = dlarrc;
