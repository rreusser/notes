

// TypeScript declarations for @stdlib/lapack/base/dsbtrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a real symmetric band matrix to tridiagonal form by orthogonal similarity transformation.
	*/
	(
		vect: string,
		uplo: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Reduces a real symmetric band matrix to tridiagonal form by orthogonal similarity transformation.
*/
declare var dsbtrd: Routine;

export = dsbtrd;
