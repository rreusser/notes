/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

/**
* Compute the minimum norm solution using SVD.
*
* @module @stdlib/lapack/base/dgelss
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgelss = require( '@stdlib/lapack/base/dgelss' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var S = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* 
* dgelss( 'row-major', 2, 2, 1, A, 2, B, 2, S, 1, 1.0, 1.0, WORK, 1, 8 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgelss = require( '@stdlib/lapack/base/dgelss' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var s = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* 
* dgelss.ndarray( 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0, s, 1, 0, 1.0, 2, WORK, 1, 0, 8 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dgelss.ndarray" }
