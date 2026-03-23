'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztrsm = require( './ztrsm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrsm, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrsm;
