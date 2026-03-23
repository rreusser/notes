

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var drot = require( './drot.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( drot, 'ndarray', ndarray );


// EXPORTS //

module.exports = drot;
