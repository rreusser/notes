'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhesv = require( './zhesv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhesv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhesv;
