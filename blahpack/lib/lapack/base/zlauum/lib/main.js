

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlauum = require( './zlauum.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlauum, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlauum;
