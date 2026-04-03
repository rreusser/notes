

// TypeScript declarations for @stdlib/lapack/base/zlaqsy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Scales a symmetric/Hermitian matrix using scaling factors computed by zpoequ.
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
* Scales a symmetric/Hermitian matrix using scaling factors computed by zpoequ.
*/
declare var zlaqsy: Routine;

export = zlaqsy;
