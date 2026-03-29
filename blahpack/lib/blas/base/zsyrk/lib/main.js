'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyrk = require( './zsyrk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyrk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyrk;
