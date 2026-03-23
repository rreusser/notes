

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztrtri = require( './ztrtri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrtri, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrtri;
