/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztprfb = require( './ztprfb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztprfb, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztprfb;
