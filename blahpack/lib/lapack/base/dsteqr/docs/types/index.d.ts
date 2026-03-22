

// TypeScript declarations for @stdlib/lapack/base/dsteqr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvalues and eigenvectors of a symmetric tridiagonal matrix
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
* Compute eigenvalues and eigenvectors of a symmetric tridiagonal matrix
*/
declare var dsteqr: Routine;

export = dsteqr;
