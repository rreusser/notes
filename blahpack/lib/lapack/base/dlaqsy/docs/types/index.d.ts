

// TypeScript declarations for @stdlib/lapack/base/dlaqsy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Equilibrate a symmetric matrix using scaling factors
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number,
		equed: string
	): Float64Array;
}

/**
* Equilibrate a symmetric matrix using scaling factors
*/
declare var dlaqsy: Routine;

export = dlaqsy;
