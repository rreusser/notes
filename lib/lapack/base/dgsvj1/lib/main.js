/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgsvj1 = require( './dgsvj1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgsvj1, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgsvj1;
