

// TypeScript declarations for @stdlib/lapack/base/zlatbs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex triangular banded solve with scaling
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		normin: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		scale: number,
		CNORM: Float64Array,
		strideCNORM: number,
		offsetCNORM: number
	): Float64Array;
}

/**
* Complex triangular banded solve with scaling
*/
declare var zlatbs: Routine;

export = zlatbs;
