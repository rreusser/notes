
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgexc = require( './ztgexc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgexc, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgexc;
