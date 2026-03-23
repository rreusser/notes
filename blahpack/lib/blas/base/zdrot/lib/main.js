

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zdrot = require( './zdrot.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zdrot, 'ndarray', ndarray );


// EXPORTS //

module.exports = zdrot;
