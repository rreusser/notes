

// TypeScript declarations for @stdlib/lapack/base/dgghrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations.
	*/
	(
		compq: string,
		compz: string,
		N: number,
		ilo: number,
		ihi: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number
	): Float64Array;
}

/**
* Reduces a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations.
*/
declare var dgghrd: Routine;

export = dgghrd;
