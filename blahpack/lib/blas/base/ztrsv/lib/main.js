

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztrsv = require( './ztrsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrsv;
