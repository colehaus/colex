(($, argMap) => {
$(() => {

const nodeData = {
  major: {
    label: ['All men are mortal'],
    type: 'major'
  }, minor: {
    label: ['Socrates is a man'],
    type: 'minor'
  }, conclusion: {
    label: ['Socrates is mortal'],
    type: 'conclusion'
  }
};
for (let prop in nodeData) {
  if(nodeData.hasOwnProperty(prop)) {
    nodeData[prop].url = '#' + prop;
  }
}
const linkData = [
  {source: 'major', target: 'conclusion', type: 'imply'},
  {source: 'minor', target: 'conclusion', type: 'imply'}
];

const nodeTypeData = [
  {type: 'major', label: ['Major premise'], shape: argMap.square},
  {type: 'minor', label: ['Minor premise'], shape: argMap.diamond},
  {type: 'conclusion', label: ['Conclusion'], shape: argMap.circle}
];

const linkTypeData = [{type: 'imply', label: ['Jointly implies']}];

argMap.handler(argMap.mkMap('#arg-map',
                            nodeData,
                            linkData,
                            nodeTypeData,
                            linkTypeData));
  
});
})($, argMap);
