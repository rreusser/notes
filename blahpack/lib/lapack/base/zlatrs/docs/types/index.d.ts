

// TypeScript declarations for @stdlib/lapack/base/zlatrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a complex triangular system with scaling to prevent overflow
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		normin: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
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
* Solves a complex triangular system with scaling to prevent overflow
*/
declare var zlatrs: Routine;

export = zlatrs;
