

// TypeScript declarations for @stdlib/lapack/base/dstev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute all eigenvalues and optionally eigenvectors of a real symmetric tridiagonal matrix
	*/
	(
		jobz: string,
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
* Compute all eigenvalues and optionally eigenvectors of a real symmetric tridiagonal matrix
*/
declare var dstev: Routine;

export = dstev;
