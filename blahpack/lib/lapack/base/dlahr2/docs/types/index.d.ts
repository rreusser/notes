

// TypeScript declarations for @stdlib/lapack/base/dlahr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce NB columns of a general matrix in Hessenberg form
	*/
	(
		N: number,
		K: number,
		nb: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		tau: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		t: Float64Array,
		strideT: number,
		offsetT: number,
		ldt: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		ldy: number
	): Float64Array;
}

/**
* Reduce NB columns of a general matrix in Hessenberg form
*/
declare var dlahr2: Routine;

export = dlahr2;
