

// TypeScript declarations for @stdlib/lapack/base/dpteqr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a real symmetric positive definite tridiagonal matrix.
	*/
	(
		compz: string,
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Computes all eigenvalues and optionally eigenvectors of a real symmetric positive definite tridiagonal matrix.
*/
declare var dpteqr: Routine;

export = dpteqr;
