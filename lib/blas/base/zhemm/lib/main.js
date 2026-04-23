'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhemm = require( './zhemm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhemm, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhemm;
